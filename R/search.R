#' @title
#' Search for a Code in the Concept Table
#'
#' @inheritParams queryAthena
#' @inheritParams grepl
#'
#' @return resultset as a dataframe with all column types as character and trimmed white space
#' @export
#' @rdname searchCode

searchCode <-
        function(code_pattern,
                 ignore.case = FALSE,
                 perl = FALSE,
                 fixed = FALSE,
                 useBytes = FALSE,
                 schema,
                 limit = NULL,
                 verbose = FALSE,
                 conn = NULL,
                 conn_fun,
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 render_sql = FALSE,
                 sleepTime = 1) {


                        code <- stringr::str_replace_all(string = code_pattern,
                                                         pattern = "[^0-9A-Za-z]{1,}",
                                                         replacement = "%")

                        sql_statement <-
                                pg13::buildQueryLike(schema = schema,
                                                     tableName = "concept",
                                                     whereLikeField = "concept_code",
                                                     whereLikeValue = code,
                                                     caseInsensitive = TRUE,
                                                     limit_n = limit)

                        resultset <- queryAthena(sql_statement = sql_statement,
                                            conn = conn,
                                            conn_fun = conn_fun,
                                            cache_only = cache_only,
                                            skip_cache = skip_cache,
                                            override_cache = override_cache,
                                            render_sql = render_sql,
                                            verbose = verbose,
                                            sleepTime = sleepTime)

                        resultset %>%
                                dplyr::filter_at(dplyr::vars(concept_code),
                                                 dplyr::all_vars(grepl(pattern = code_pattern,
                                                                       x = .,
                                                                       ignore.case = ignore.case,
                                                                       perl = perl,
                                                                       fixed = fixed,
                                                                       useBytes = useBytes)))
        }
