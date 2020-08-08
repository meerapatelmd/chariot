#' @title Join a data frame with the Concept Synonym Table
#' @description
#' This function has better performance than the WHERE IN statement for larger searches. A table in the format of "v{unpunctuated timestamp}" is written to the local Athena. A join is performed with a concept table. The table is subsequently dropped. If a dataframe column is not provided as a the column to join the concept table on, the 1st column will be used by default.
#'
#' @param .data                 A data frame
#' @param column                Data frame column that the join will be performed on. If NULL, defaults to the column in position 1 of the data frame.
#' @param athena_schema         Schema of the OMOP Vocabulary Tables
#' @param verbose               If TRUE, prints whether the cache is being loaded or being actively queried in the Postgres database, Default: FALSE
#' @param conn                  PARAM_DESCRIPTION, Default: NULL
#' @param render_sql            If TRUE, will print the SQL to the console before executing. Default: FALSE
#' @param sleepTime             Argument in seconds passed to the `Sys.sleep()` function at the end of query, Default: 1
#' @param ...                   Additional arguments passed to the `queryAthena()` function.
#'
#' @return
#' A data frame
#'
#' @seealso
#'  \code{\link[dplyr]{select}}
#' @rdname leftJoinSynonymId
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr select


leftJoinSynonymId <-
    function(.data,
             column = NULL,
             athena_schema,
             verbose = FALSE,
             conn = NULL,
             render_sql = FALSE,
             sleepTime = 1,
             ...) {


                            if (is.null(column)) {
                                column <- colnames(.data)[1]
                            }


                            if (column == "concept_id") {
                                stop("'column' parameter cannot be equal to 'concept_id'")
                            }


                            leftJoin(.data = .data,
                                      column = column,
                                      athena_schema = athena_schema,
                                      athena_table = "concept_synonym",
                                      athena_column = "concept_id",
                                      render_sql = render_sql,
                                      where_athena_col = "language_concept_id",
                                      where_athena_col_in = 4180186,
                                      verbose = verbose,
                                      conn = conn,
                                      render_sql = render_sql,
                                      sleepTime = sleepTime,
                                      ...) %>%
                                    dplyr::select(-language_concept_id)

    }

