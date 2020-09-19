#' @title Left Join a data frame to the Concept Ancestor Table
#' @param .data PARAM_DESCRIPTION
#' @param athena_schema PARAM_DESCRIPTION, Default: 'public'
#' @param descendant_id_column PARAM_DESCRIPTION, Default: NULL
#' @param whereLevelIn PARAM_DESCRIPTION, Default: NULL
#' @param whereLevelType PARAM_DESCRIPTION, Default: NULL
#' @param verbose PARAM_DESCRIPTION, Default: FALSE
#' @param conn PARAM_DESCRIPTION, Default: NULL
#' @param render_sql PARAM_DESCRIPTION, Default: FALSE
#' @param sleepTime PARAM_DESCRIPTION, Default: 1
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @seealso
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{distinct}},\code{\link[dplyr]{mutate-joins}}
#'  \code{\link[rubix]{rename_all_with_prefix}}
#' @rdname leftJoinForAncestors
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr select distinct left_join
#' @importFrom rubix rename_all_with_prefix

leftJoinForAncestors <-
        function(.data,
                 athena_schema = "public",
                 descendant_id_column = NULL,
                 whereLevelIn = NULL,
                 whereLevelType = NULL,
                 verbose = FALSE,
                 conn = NULL,
                 render_sql = FALSE,
                 sleepTime = 1,
                 ...) {

                        if (!is.null(whereLevelIn) && length(whereLevelType) != 1) {


                                warning("No 'whereLevelType'. Defaulting to 'max'")
                                whereLevelType <- "max"

                        }

                        if (!is.null(whereLevelIn)) {

                                if (whereLevelType == "max") {
                                        whereAthenaField <- "max_levels_of_separation"
                                } else {
                                        whereAthenaField <- "min_levels_of_separation"
                                }


                                ancestors <-
                                        leftJoin(.data = .data,
                                                 column = descendant_id_column,
                                                 athena_schema = athena_schema,
                                                 athena_table = "concept_ancestor",
                                                 athena_column = "descendant_concept_id",
                                                 where_athena_col = whereAthenaField,
                                                 where_athena_col_in = whereLevelIn,
                                                 verbose = verbose,
                                                 conn = conn,
                                                 render_sql = render_sql,
                                                 sleepTime = sleepTime,
                                                 ...)

                        } else {

                                ancestors <-
                                        leftJoin(.data = .data,
                                                 column = descendant_id_column,
                                                 athena_schema = athena_schema,
                                                 athena_table = "concept_ancestor",
                                                 athena_column = "descendant_concept_id",
                                                 verbose = verbose,
                                                 conn = conn,
                                                 render_sql = render_sql,
                                                 sleepTime = sleepTime,
                                                 ...)
                        }



                                ancestors_detail <-
                                        leftJoinConcept(ancestors %>%
                                                                dplyr::select(ancestor_concept_id),
                                                        athena_schema = athena_schema,
                                                        synonyms = FALSE,
                                                        verbose = verbose,
                                                        conn = conn,
                                                        render_sql = render_sql,
                                                        sleepTime = sleepTime,
                                                        ...) %>%
                                        dplyr::select(-ancestor_concept_id) %>%
                                        rubix::rename_all_with_prefix("ancestor_") %>%
                                        dplyr::distinct()


                                final_ancestors <-
                                        dplyr::left_join(ancestors,
                                                         ancestors_detail,
                                                         by = "ancestor_concept_id") %>%
                                        dplyr::select(-descendant_concept_id)


                                return(final_ancestors)

        }
