#' Read SQL from Installation Directory
#' @noRd

read_sql_template <-
  function(file) {
    readLines(
      system.file(package = "chariot",
                  "sql",
                  file)) %>%
      paste(collapse = "\n")
  }
