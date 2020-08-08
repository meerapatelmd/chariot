#' @title Query concept children
#' @seealso
#'  \code{\link[pg13]{buildQuery}}
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{select}}
#'  \code{\link[purrr]{keep}}
#' @rdname queryConceptParent
#' @export
#' @impotFrom magrittr %>%
#' @importFrom pg13 buildQuery
#' @importFrom dplyr filter select
#' @importFrom purrr keep


queryConceptParent <-
    function(child_id,
             schema,
             generations = 1,
             verbose = FALSE,
             cache_resultset = TRUE,
             override_cache = FALSE,
             conn = NULL,
             render_sql = FALSE,
             sleepTime = 1,
             ...) {

                sql_statement <- pg13::buildQuery(schema = schema,
                                                  tableName = "concept_parent",
                                                  whereInField = "child_concept_id",
                                                  whereInVector = child_id,
                                                  caseInsensitive = FALSE)

                baseline <-
                        queryAthena(sql_statement = sql_statement,
                                    verbose = verbose,
                                    cache_resultset = cache_resultset,
                                    override_cache = override_cache,
                                    conn = conn,
                                    render_sql = render_sql,
                                    sleepTime = sleepTime,
                                    ...) %>%
                        dplyr::filter(parent_concept_id != child_concept_id)

                if (nrow(baseline) == 0) {

                        message('concept "', child_id, '" does not have parents')
                        return(baseline)

                }

                output <- list()
                output[[1]] <- baseline


                if (generations > 1) {

                        for (i in 2:generations) {
                                prior <- output[[(i-1)]]

                                if (!is.null(prior)) {

                                        if (nrow(prior) > 0) {

                                                        #Prior child will now be the new parent
                                                        prior <-
                                                                prior %>%
                                                                dplyr::select(new_child_concept_id = parent_concept_id)

                                                        output[[i]] <-
                                                                leftJoinForParents(.data = prior,
                                                                           athena_schema = schema,
                                                                           child_id_column = "new_child_concept_id",
                                                                           render_sql = render_sql,
                                                                           conn = conn) %>%
                                                                dplyr::select(-any_of("new_child_concept_id"))

                                        } else {

                                                output[[i]] <- NULL

                                        }

                                } else {

                                        output[[i]] <- NULL

                                }


                        }

                }

                output <-
                        output %>%
                        purrr::keep(~!is.null(.)) %>%
                        purrr::keep(~nrow(.)>0)


                output <-
                        output[length(output):1]

                if (length(output) != generations) {

                    message('Maximum possible generations less than "generations" param:', length(output))
                }

                return(output)

    }


