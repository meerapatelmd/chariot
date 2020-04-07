#' Returns all possible OMOP domains in Athena
#' @return dataframe of unique `domain_id` variables from the `concept` table
#' @importFrom mySeagull connect_to_local_postgres
#' @import DBI
#' @export

return_omop_classes <-
        function() {
                resultset <- query_athena("SELECT DISTINCT concept_class_id FROM public.concept;")
                return(resultset)

        }
