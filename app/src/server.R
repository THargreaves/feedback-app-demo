library(conflicted)
library(DBI)
library(dplyr)
library(dbplyr)
library(digest)
library(shiny)
library(shinyWidgets)
library(tibble)
library(tidyr)

conflict_prefer("filter", "dplyr")

# TODO: add error handling for hash collisions
# TODO: use vote_id for voting in case hash doesn't exist
server <- function(input, output, session) {

  # TODO: refactor into a single reactive value
  proposal_id <- reactive({
    parseQueryString(session$clientData$url_search)$proposal_id
  })
  hash <- reactive({
    parseQueryString(session$clientData$url_search)$hash
  })
  action <- reactive({
    parseQueryString(session$clientData$url_search)$action
  })

  output$proposals_ui <- renderUI({
    if (is.null(proposal_id())) {
      box(title = "All Proposals", width = 12,
        dataTableOutput("proposals_table")
      )
    } else {
      proposal <- pool |>
        tbl("proposal") |>
        filter(proposal_id == !!proposal_id()) |>
        collect()
      
      # TODO: refactor this unholy mess
      # TODO: hide unapproved proposals
      if (nrow(proposal) == 0) {
        sendSweetAlert(
          session,
          title = "Proposal Not Found",
          text = str_c("There is no proposal with ID ", proposal_id()),
          type = "error",
          btn_labels = HTML("<a href='/' style='color:white;text-decoration:none'>Go Back</a>"),
          closeOnClickOutside = FALSE
        )
      } else if (!is.null(action()) && action() == "approve") {
        if (!hash() == proposal$hash) {
          sendSweetAlert(
            session,
            title = "Invalid Hash",
            text = str_c("The supplied verification hash is not valid"),
            type = "error",
            btn_labels = HTML("<a href='/' style='color:white;text-decoration:none'>Go Back</a>"),
            closeOnClickOutside = FALSE
          )
        } else {
          status <- case_when(
            is.na(proposal$approved) ~ "Pending",
            proposal$approved ~ "Approved",
            TRUE ~ "Disapproved"
          )
          box(title = str_c("Proposal #", proposal_id()), width = 12,
            actionButton("approve_btn", "Approve", icon = icon("check-circle")),
            actionButton("disapprove_btn", "Disapprove", icon = icon("times-circle")),
            tags$h3(str_c(proposal$title, " [Status:", status ,"]")),
            tags$p(proposal$body)
          )
        }
      } else {
        if (!is.null(action()) && action() == "vote") {
          vote = pool |>
            tbl("vote") |>
            filter(hash == !!hash()) |>
            collect()
          if (vote$proposal_id != proposal$proposal_id) {
            sendSweetAlert(
              session,
              title = "Invalid Proposal ID",
              text = str_c("The proposal ID does not match the vote ID"),
              type = "error"
            )
          } else if (!hash() == vote$hash) {
            sendSweetAlert(
              session,
              title = "Invalid Hash",
              text = str_c("The supplied verification hash is not valid"),
              type = "error"
            )
          } else {
            dbExecute(pool, str_c("
              UPDATE vote
              SET verified = ", 1, "
              WHERE vote_id = ", vote$vote_id
            ))
            sendSweetAlert(
              session,
              title = "Voting Successful",
              text = "Your vote has been successfully verified",
              type = "success"
            )
          }
        }
        votes <- pool |>
          tbl("vote") |>
          filter(proposal_id == proposal_id, verified) |>
          collect() %>%
          nrow()

        box(title = str_c("Proposal #", proposal_id()), width = 12,
          actionButton("view_all_btn", "View All", icon = icon("arrow-left"),
                      onclick ="location.href='/';"),
          actionButton("vote_btn", "Vote", icon = icon("vote-yea")),
          actionButton("subscribe_btn", "Subscribe", icon = icon("bell")),
          tags$h3(str_c(proposal$title, " [", votes, " vote(s)]")),
          tags$p(proposal$body)
        )
      }
    }
  })

  output$proposals_table <- renderDataTable({
    votes <- pool |>
      tbl("vote") |>
      filter(verified) |>
      select(proposal_id, vote_id)

    pool |>
      tbl("proposal") |>
      filter(approved) |>
      left_join(votes, by = "proposal_id") |>
      group_by(proposal_id, datetime, title) |>
      summarise(votes = sum(vote_id, na.rm = TRUE), .groups = "drop") |>
      collect() |>  # need to collect before using local function
      mutate(
        title = make_link(proposal_id, title),
        votes = replace_na(votes, 0),
        datetime = as.Date(datetime)
      ) |>
      rename(date = datetime) |>
      rename_all(str_to_title) |>
      rename(ID = Proposal_id)
  }, escape = FALSE)

  observeEvent(input$approve_btn, {
    change_approval_status(proposal_id(), TRUE, session)
  })

  observeEvent(input$disapprove_btn, {
    change_approval_status(proposal_id(), FALSE, session)
  })

  observeEvent(input$vote_btn, {
    inputSweetAlert(
      session,
      "warwick_id",
      "Enter your Warwick ID",
      "Your Warwick ID is used to verify your vote by email and is not stored by the app",
      type = "question",
      input = "text"
    )
  })

  observeEvent(input$warwick_id, {
    print(input$warwick_id)
    new_hash <- digest(str_c(input$warwick_id, proposal_id(), salt), algo="md5")
    match <- pool |>
      tbl("vote") |>
      filter(hash == new_hash) |>
      collect() |>
      nrow() > 0
    
    if (match) {
      # TODO: resend email
      sendSweetAlert(
        session,
        title = "Repeat Vote",
        text = "You have already voted for this proposal",
        type = "info"
      )
    } else {
      tryCatch({
        # Create vote dataframe
        submission <- tibble_row(
          proposal_id = proposal_id(),
          hash = new_hash,
          verified = 0
        )
        dbAppendTable(pool, "vote", submission)
        # Create approval link and notify user of success
        link <- str_glue(
          "/?proposal_id={id}&action=vote&hash={hash}",
          id = proposal_id(), hash = new_hash
        )
        sendSweetAlert(
          session,
          title = "Submission Successful",
          text = HTML(str_glue(
            "Your vote will be counted once it is verified ",
            "[<a href='{link}'>link</a>]",
            link = link
          )),
          type = "success",
          html = TRUE
        )
      }, error = function(e) {
        sendSweetAlert(
          session,
          title = "Voting Failed",
          text = "Please contact the app maintainer",
          type = "error"
        )
        print(e)
      })
    }
  })

  observeEvent(input$submission_submit, {
    tryCatch({
      # Generate approval hash
      next_id <- get_next_id("proposal")
      hash <- digest(str_c(next_id, salt), algo="md5")
      # Create submission dataframe
      submission <- tibble_row(
        datetime = Sys.time(),
        title = input$submission_title,
        body = input$submission_body,
        hash = hash
      )
      dbAppendTable(pool, "proposal", submission)
      # Create approval link and notify user of success
      link <- str_glue(
        "/?proposal_id={id}&action=approve&hash={hash}",
        id = next_id, hash = hash
      )
      sendSweetAlert(
        session,
        title = "Submission Successful",
        text = HTML(str_glue(
          "Your proposal will be displayed once it is approved ",
          "[<a href='{link}'>link</a>]",
          link = link
        )),
        type = "success",
        html = TRUE
      )
    }, error = function(e) {
      sendSweetAlert(
        session,
        title = "Submission Failed",
        text = "Please contact the app maintainer",
        type = "error"
      )
      print(e)
    })
  })
}
