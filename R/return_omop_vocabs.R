#' Returns all possible OMOP domains in Athena
#' @return dataframe of unique `domain_id` variables from the `concept` table
#' @export

return_omop_vocabs <-
        function() {
                resultset <- query_athena("SELECT DISTINCT vocabulary_id FROM public.concept;")
                return(resultset)

        }
