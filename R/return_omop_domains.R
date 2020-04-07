#' Returns all possible OMOP domains in Athena
#' @return dataframe of unique `domain_id` variables from the `concept` table
#' @importFrom mySeagull connect_to_local_postgres
#' @import DBI
#' @export

return_omop_domains <-
        function() {
                resultset <- query_athena("SELECT DISTINCT domain_id FROM public.concept;")
                return(resultset)

        }
