#' Join a dataframe object with Athena's concept table
#' @description This function has better performance than the WHERE IN statement for larger searches. A table in the format of "v{unpunctuated timestamp}" is written to the local Athena. A join is performed with a concept table. The table is subsequently dropped. If a dataframe column is not provided as a the column to join the concept table on, the 1st column will be used by default.
#' @param dataframe dataframe to join
#' @param dataframe_column string of the column name to join on. If NULL, the 1st column is use
#' @param concept_column name of concept_column to join dataframe on. Defaults to concept ID.
#' @export


leftJoinSynonymId <-
    function(.data,
             column = NULL,
             athena_schema = "public",
             render_sql = TRUE,
             conn = NULL) {


                            if (is.null(column)) {
                                column <- colnames(.data)[1]
                            }


                            if (column == "concept_id") {
                                stop("'column' parameter cannot be equal to 'concept_id'")
                            }


                            leftJoin( .data = .data,
                                      column = column,
                                      athena_schema = athena_schema,
                                      athena_table = "concept_synonym",
                                      athena_column = "concept_id",
                                      render_sql = render_sql,
                                      conn = conn) %>%
                            dplyr::filter(language_concept_id == 4180186) %>%
                            dplyr::select(-language_concept_id)

    }

