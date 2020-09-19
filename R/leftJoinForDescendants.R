#' @title Left Join a data frame to the Concept Ancestor Table
#' @description FUNCTION_DESCRIPTION
#' @param .data PARAM_DESCRIPTION
#' @param athena_schema Default: 'public'
#' @param ancestor_id_column Default: NULL
#' @param whereLevelIn Default: NULL
#' @param whereLevelType Default: NULL
#' @param render_sql Default: TRUE
#' @param conn Default: NULL
#' @seealso
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{distinct}},\code{\link[dplyr]{mutate-joins}}
#'  \code{\link[rubix]{rename_all_with_prefix}}
#' @rdname leftJoinForDescendants
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr select distinct left_join
#' @importFrom rubix rename_all_with_prefix

leftJoinForDescendants <-
        function(.data,
                 athena_schema = "public",
                 ancestor_id_column = NULL,
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


                                descendants <-
                                        leftJoin(.data = .data,
                                                 column = ancestor_id_column,
                                                 athena_schema = athena_schema,
                                                 athena_table = "concept_ancestor",
                                                 athena_column = "ancestor_concept_id",
                                                 where_athena_col = whereAthenaField,
                                                 where_athena_col_in = whereLevelIn,
                                                 verbose = verbose,
                                                 conn = conn,
                                                 render_sql = render_sql,
                                                 sleepTime = sleepTime,
                                                 ...)

                        } else {

                                descendants <-
                                        leftJoin(.data = .data,
                                                 column = ancestor_id_column,
                                                 athena_schema = athena_schema,
                                                 athena_table = "concept_ancestor",
                                                 athena_column = "ancestor_concept_id",
                                                 verbose = verbose,
                                                 conn = conn,
                                                 render_sql = render_sql,
                                                 sleepTime = sleepTime,
                                                 ...)
                        }



                                descendants_detail <-
                                        leftJoinConcept(descendants %>%
                                                                dplyr::select(descendant_concept_id),
                                                        athena_schema = athena_schema,
                                                        synonyms = FALSE,
                                                        verbose = verbose,
                                                        conn = conn,
                                                        render_sql = render_sql,
                                                        sleepTime = sleepTime,
                                                        ...) %>%
                                        dplyr::select(-descendant_concept_id) %>%
                                        rubix::rename_all_with_prefix("descendant_") %>%
                                        dplyr::distinct()


                                final_descendants <-
                                        dplyr::left_join(descendants,
                                                         descendants_detail,
                                                         by = "descendant_concept_id") %>%
                                        dplyr::select(-ancestor_concept_id)


                                return(final_descendants)

        }
