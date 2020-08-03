#' Join a dataframe object with Athena's concept table
#' @description This function has better performance than the WHERE IN statement for larger searches. A table in the format of "v{unpunctuated timestamp}" is written to the local Athena. A join is performed with a concept table. The table is subsequently dropped. If a dataframe column is not provided as a the column to join the concept table on, the 1st column will be used by default.
#' @param dataframe dataframe to join
#' @param column string of the column name to join on. If NULL, the 1st column is used.
#' @param athena_column name of column to join dataframe on. Defaults to concept ID.

# left_join_df <-
#     function(.data,
#              column = NULL,
#              athena_table,
#              athena_column,
#              where_athena_col = NULL,
#              where_athena_col_equals = NULL,
#              override_cache = FALSE) {
#
#
#         if (override_cache) {
#                 table_name <- paste0("v", stampede::stamp_this(without_punct = TRUE))
#                 if (is.null(column)) {
#                     column <- colnames(.data)[1]
#                 }
#
#                 conn <- connect_athena()
#                 DatabaseConnector::dbWriteTable(conn = conn,
#                                                 name = table_name,
#                                                 value = .data %>%
#                                                     as.data.frame())
#                 dc_athena(conn = conn)
#
#                 if (is.null(where_athena_col)) {
#                     output <- query_athena(paste0("SELECT * FROM ", table_name, " LEFT JOIN ", athena_table, " c ON c.", athena_column, " = ", column),
#                                            cache_resultset = FALSE)
#
#                 } else {
#
#                     output <- query_athena(paste0("SELECT * FROM ", table_name, " LEFT JOIN ", athena_table, " c ON c.", athena_column, " = ", column, " WHERE c.", where_athena_col, " IN ", seagull::write_where_in_string(where_athena_col_equals)),
#                                            cache_resultset = FALSE)
#
#
#                 }
#
#                 cache_left_join(object=output,
#                                 vector=.data %>%
#                                     dplyr::select(all_of(column)) %>%
#                                     unlist() %>%
#                                     unname(),
#                                 athena_table = athena_table,
#                                 athena_column = athena_column,
#                                 where_athena_col = where_athena_col,
#                                 where_athena_col_equals = where_athena_col_equals,
#                                 omop=FALSE,
#                                 omop_schema=NULL)
#
#                 drop_left_join_tables()
#
#
#
#
#         } else {
#
#                 table_name <- paste0("v", stampede::stamp_this(without_punct = TRUE))
#                 if (is.null(column)) {
#                     column <- colnames(.data)[1]
#                 }
#
#
#                 cached_resultset <-
#                         load_cached_left_join(vector = .data %>%
#                                                     dplyr::select(all_of(column)) %>%
#                                                     unlist() %>%
#                                                     unname(),
#                                          athena_table = athena_table,
#                                          athena_column = athena_column,
#                                          where_athena_col = where_athena_col,
#                                          where_athena_col_equals = where_athena_col_equals,
#                                          omop=FALSE,
#                                          omop_schema=NULL)
#
#
#                 if (is.null(cached_resultset)) {
#
#
#                                 conn <- connect_athena()
#                                 DatabaseConnector::dbWriteTable(conn = conn,
#                                                                 name = table_name,
#                                                                 value = .data %>%
#                                                                             as.data.frame())
#                                 dc_athena(conn = conn)
#
#                             if (is.null(where_athena_col)) {
#                                     output <- query_athena(paste0("SELECT * FROM ", table_name, " LEFT JOIN ", athena_table, " c ON c.", athena_column, " = ", column),
#                                                            cache_resultset = FALSE)
#
#                             } else {
#
#                                         output <- query_athena(paste0("SELECT * FROM ", table_name, " LEFT JOIN ", athena_table, " c ON c.", athena_column, " = ", column, " WHERE c.", where_athena_col, " IN ", seagull::write_where_in_string(where_athena_col_equals)),
#                                                                cache_resultset = FALSE)
#
#
#                             }
#
#                                 cache_left_join(object=output,
#                                                 vector=.data %>%
#                                                     dplyr::select(all_of(column)) %>%
#                                                     unlist() %>%
#                                                     unname(),
#                                                 athena_table = athena_table,
#                                                 athena_column = athena_column,
#                                                 where_athena_col = where_athena_col,
#                                                 where_athena_col_equals = where_athena_col_equals,
#                                                 omop=FALSE,
#                                                 omop_schema=NULL)
#
#                             drop_left_join_tables()
#
#
#                 } else {
#                 output <- cached_resultset
#             }
#
#         }
#
#         return(output)
#     }
