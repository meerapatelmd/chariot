#' Join a dataframe object with Athena's concept table
#' @description This function has better performance than the WHERE IN statement for larger searches. A table in the format of "v{unpunctuated timestamp}" is written to the local Athena. A join is performed with a concept table. The table is subsequently dropped. If a dataframe column is not provided as a the column to join the concept table on, the 1st column will be used by default.
#' @param dataframe dataframe to join
#' @param column string of the column name to join on. If NULL, the 1st column is used.
#' @param athena_column name of column to join dataframe on. Defaults to concept ID.

#'
#' left_join_df_omop <-
#'     function(.data,
#'              column = NULL,
#'              omop_schema = "omop_vocabulary",
#'              athena_table,
#'              athena_column,
#'              where_athena_col = NULL,
#'              where_athena_col_equals = NULL) {
#'
#'
#'                 table_name <- paste0("Meera_v", stampede::stamp_this(without_punct = TRUE))
#'
#'                 if (is.null(column)) {
#'
#'                     column <- colnames(.data)[1]
#'
#'                 }
#'
#'                 # conn <- fantasia::connect_to_omop(schema = omop_schema)
#'                 # DatabaseConnector::dbWriteTable(conn = conn,
#'                 #                                 name = table_name,
#'                 #                                 value = .data %>%
#'                 #                                             as.data.frame())
#'                 # fantasia::disconnect_omop(conn = conn)
#'
#'             if (is.null(where_athena_col)) {
#'
#'
#'                 output <-
#'                     fantasia::query_omop(schema = omop_schema,
#'                                          sql_statement = paste0("SELECT * FROM ", table_name, " LEFT JOIN ", athena_table, " c ON c.", athena_column, " = ", column),
#'                                            override_cache = FALSE)
#'
#'             } else {
#'
#'                 output <-
#'                     fantasia::query_omop(schema = omop_schema,
#'                                          sql_statement =
#'                                          paste0("SELECT * FROM ", table_name, " LEFT JOIN ", athena_table, " c ON c.", athena_column, " = ", column, " WHERE c.", where_athena_col, " IN ", seagull::write_where_in_string(where_athena_col_equals)),
#'                                                override_cache = FALSE)
#'
#'
#'             }
#'
#'                 conn <- fantasia::connect_to_omop(schema = omop_schema)
#'                 tables <- DatabaseConnector::dbListTables(conn = conn)
#'                 left_join_tables <- grep("^Meera_v[0-9]{14}$", tables, value = TRUE)
#'                 while (length(left_join_tables) > 0) {
#'                     left_join_table <- left_join_tables[1]
#'                     DatabaseConnector::dbRemoveTable(conn = conn, name = left_join_table)
#'                     left_join_tables <- left_join_tables[-1]
#'                 }
#'                 fantasia::disconnect_omop(conn = conn)
#'
#'         return(output)
#'     }
