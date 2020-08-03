#' Execute Athena constraints
#' @import police
#' @import secretary
#' @import DatabaseConnector
#' @export

execute_athena_constraints <-
    function() {


        tmp <- tempfile(fileext = ".txt")

        download.file(url = "https://raw.githubusercontent.com/OHDSI/CommonDataModel/master/PostgreSQL/OMOP%20CDM%20postgresql%20constraints.txt",
                      destfile = tmp)

        sql <- read_lines(tmp)
        sql <- grep("[;]{1}", sql, value = TRUE)

        conn <- connectAthena()

        total_statements <- length(sql)
        while (length(sql) > 0) {

            sql_line <- sql[1]

            police::try_catch_error_as_null(
                DatabaseConnector::dbSendStatement(conn = conn,
                                                   statement = sql_line)
            )


            sql <- sql[-1]

            secretary::typewrite(paste0("[", stampede::stamp_this(), "]"),
                                 total_statements-length(sql),
                                 "out of", total_statements, "completed.")

        }

        DatabaseConnector::dbDisconnect(conn = conn)
        unlink(tmp)
    }
