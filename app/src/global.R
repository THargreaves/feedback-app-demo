library(conflicted)
library(stringr)

# TODO: refactor credentials into environment variables
pool <- pool::dbPool(
  drv = RMariaDB::MariaDB(),
  dbname = "feedback",
  host = "db",
  port = 3306,
  username = "root",
  password = "temppass123"
)

salt <- Sys.getenv("SALT")

##########################
#### HELPER FUNCTIONS ####
##########################

make_link <- function(ids, titles) {
  str_glue(
    "<a href='/?proposal_id={id}'>{title}</a>",
    id = ids,
    title = titles
  )
}

get_next_id <- function(table) {
  dbGetQuery(pool, str_c("
    SELECT AUTO_INCREMENT
    FROM information_schema.tables
    WHERE table_name = '", table, "'
    AND table_schema = 'feedback';
  "))$AUTO_INCREMENT
}

change_approval_status <- function(id, approved, session) {
  tryCatch({
    dbExecute(pool, str_c("
      UPDATE proposal
      SET approved = ", as.integer(approved), "
      WHERE proposal_id = ", id
    ))
      sendSweetAlert(
        session,
        title = "Approval Status Successfully Changed",
        text = str_c("The approval status for proposal ", id, " was set to ", ifelse(approved, "approved", "disapproved")),
        type = "success",
        btn_labels = HTML("<a href='/' style='color:white;text-decoration:none'>Go Back</a>")
      )
  }, error = function(e) {
      sendSweetAlert(
        session,
        title = "Update Failed",
        text = "Please contact the app maintainer",
        type = "error"
      )
      print(e)
  })
}
