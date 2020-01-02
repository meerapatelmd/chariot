#' Returns all possible OMOP domains in Athena
#' @return dataframe of unique `domain_id` variables from the `concept` table
#' @importFrom mySeagull connect_to_local_postgres
#' @import DBI
#' @export

return_omop_domains <-
        function() {
                conn_to_athena <- mySeagull::connect_to_local_postgres(dbname = "athena")
                sql_statement <- paste0("SELECT DISTINCT domain_id FROM public.concept;")
                returnset <- DBI::dbGetQuery(conn_to_athena, sql_statement)
                return(returnset)
                DBI::dbDisconnect(conn_to_athena)

        }
