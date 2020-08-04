#' Query based on search terms that does not write to catalogue
#' @param ... vector of codes to collectively feed into the LIKE sql statement
#' @return resultset as a dataframe with all column types as character and trimmed white space
#' @export

queryCode <-
        function(code,
                 schema,
                 caseInsensitive = TRUE,
                 limit = NULL,
                 verbose = FALSE,
                 cache_resultset = TRUE,
                 override_cache = FALSE,
                 conn = NULL,
                 render_sql = FALSE,
                 type = c("like", "exact")) {

                if (length(type) != 1) {
                        warning("type is not length 1. Defaulting to 'exact'")
                        type <- "exact"
                }

                if (type == "exact") {

                        sql_statement <-
                                pg13::buildQuery(schema = schema,
                                                 tableName = "concept",
                                                 whereInField = "concept_code",
                                                 whereInVector = code,
                                                 caseInsensitive = caseInsensitive,
                                                 n = limit,
                                                 n_type = "limit")

                } else if (type == "like") {

                        sql_statement <-
                                pg13::buildQueryLike(schema = schema,
                                                     tableName = "concept",
                                                     whereLikeField = "concept_code",
                                                     whereLikeValue = code,
                                                     caseInsensitive = caseInsensitive,
                                                     limit_n = limit)

                } else {
                        stop('type not recognized: ', type)
                }

                resultset <- queryAthena(sql_statement = sql_statement,
                                         verbose = verbose,
                                         cache_resultset = cache_resultset,
                                         override_cache = override_cache,
                                         conn = conn,
                                         render_sql = render_sql)
                return(resultset)
        }
