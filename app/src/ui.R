library(conflicted)
library(shiny)
library(shinydashboard)

conflict_prefer("box", "shinydashboard")

header <- dashboardHeader(title = "Feedback App Demo")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      "View Proposals",
      tabName = "view",
      icon = icon("stream")
    ),
    menuItem(
      "Submit Proposal",
      tabName = "submit",
      icon = icon("plus-square")),
    menuItem(
      "Information",
      tabName = "info",
      icon = icon("info-circle")
    )
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "view",
      uiOutput("proposals_ui")
    ),
    tabItem(tabName = "submit",
      box(title = "Submit Proposal", width = 12,
        textInput(
          "submission_title",
          "Title"
        ),
        textAreaInput(
          "submission_body",
          "Body"
        ),
        actionButton(
          "submission_submit",
          "Submit"
        )
      )
    ),
    tabItem(tabName = "info",
      box(title = "About", width = 12),
      box(title = "Help", width = 12),
      box(title = "Roadmap", width = 12),
      box(title = "Change Log", width = 12)
    )
  )
)

dashboardPage(header, sidebar, body, skin = "yellow")
