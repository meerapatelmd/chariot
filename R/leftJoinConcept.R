#' @title Join a dataframe object with the Concept Table
#' @description
#' This function has better performance than the WHERE IN statement for larger searches. A table in the format of "v{unpunctuated timestamp}" is written to the local Athena. A join is performed with a concept table. The table is subsequently dropped. If a dataframe column is not provided as a the column to join the concept table on, the 1st column will be used by default.
#'
#' @param .data                 A data frame
#' @param column                Data frame column that the join will be performed on. If NULL, defaults to the column in position 1 of the data frame.
#' @param athena_schema         Schema of the OMOP Concept Table
#' @param concept_column        Column in the concept
#' @param verbose               If TRUE, prints whether the cache is being loaded or being actively queried in the Postgres database, Default: FALSE
#' @param conn                  Connection object if another database is used. Default: NULL
#' @param render_sql            If TRUE, will print the SQL to the console before executing. Default: FALSE
#' @param sleepTime             Argument in seconds passed to the `Sys.sleep()` function at the end of query, Default: 1
#' @param ...                   Additional arguments passed to the `queryAthena()` function.
#'
#' @return
#' A data frame
#'
#' @seealso
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{filter}},\code{\link[dplyr]{mutate-joins}}
#'  \code{\link[rubix]{group_by_unique_aggregate}}
#' @rdname leftJoinConcept
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr select filter left_join
#' @importFrom rubix group_by_unique_aggregate


leftJoinConcept <-
    function(.data,
             column = NULL,
             athena_schema = "public",
             concept_column = "concept_id",
             synonyms = FALSE,
             verbose = FALSE,
             conn = NULL,
             render_sql = FALSE,
             sleepTime = 1) {


                            if (is.null(column)) {
                                column <- colnames(.data)[1]
                            }

                            if (column == concept_column) {
                                stop("'column' parameter cannot be equal to 'concept_column'")
                            }

                            output <-
                                leftJoin(.data = .data,
                                         column = column,
                                         athena_schema = athena_schema,
                                         athena_table = "concept",
                                         athena_column = concept_column,
                                         verbose = verbose,
                                         conn = conn,
                                         render_sql = render_sql,
                                         sleepTime = sleepTime)


                            if (synonyms) {

                                    output_s <-
                                            leftJoinSynonymId(.data = output %>%
                                                                        dplyr::select(-concept_id),
                                                              column = column,
                                                              athena_schema = athena_schema,
                                                              verbose = verbose,
                                                              conn = conn,
                                                              render_sql = render_sql,
                                                              sleepTime = sleepTime) %>%
                                            dplyr::filter(concept_name != concept_synonym_name) %>%
                                            rubix::group_by_unique_aggregate(concept_id,
                                                                             agg.col = concept_synonym_name,
                                                                             collapse = "|")


                                    output <-
                                            output %>%
                                            dplyr::left_join(output_s)

                            }



                    return(output)

    }
