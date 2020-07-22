#' Query a HemOnc Regimen's 'Has antineoplastic' Relationship
#' @export

queryHemOncRegToAntineo <-
        function(regimen_concept_ids,
                 schema = NULL,
                 ...) {

                sql_statement <-
                        renderHemOncRegToAntineoplastics(regimen_concept_ids = regimen_concept_ids,
                                                         schema = schema)
                resultset <-
                        query_athena(sql_statement = sql_statement,
                                     ...)

                return(resultset)
        }
