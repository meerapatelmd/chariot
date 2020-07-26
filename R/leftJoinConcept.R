#' Join a dataframe object with Athena's concept table
#' @description This function has better performance than the WHERE IN statement for larger searches. A table in the format of "v{unpunctuated timestamp}" is written to the local Athena. A join is performed with a concept table. The table is subsequently dropped. If a dataframe column is not provided as a the column to join the concept table on, the 1st column will be used by default.
#' @param dataframe dataframe to join
#' @param dataframe_column string of the column name to join on. If NULL, the 1st column is use
#' @param concept_column name of concept_column to join dataframe on. Defaults to concept ID.
#' @importFrom seagull create_table_via_temp_file
#' @importFrom seagull drop_table
#' @export


leftJoinConcept <-
    function(.data,
             column = NULL,
             athena_schema = "public",
             concept_column = "concept_id") {

            leftJoin(.data = data,
                     column = column,
                     athena_schema = athena_schema,
                     athena_table = "concept",
                     athena_column = concept_column)

    }
