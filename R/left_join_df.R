#' Join a dataframe object with Athena's concept table
#' @description This function has better performance than the WHERE IN statement for larger searches. A table in the format of "v{unpunctuated timestamp}" is written to the local Athena. A join is performed with a concept table. The table is subsequently dropped. If a dataframe column is not provided as a the column to join the concept table on, the 1st column will be used by default.
#' @param dataframe dataframe to join
#' @param dataframe_column string of the column name to join on. If NULL, the 1st column is used.
#' @param athena_column name of column to join dataframe on. Defaults to concept ID.
#' @importFrom seagull create_table_via_temp_file
#' @importFrom seagull drop_table
#' @export

left_join_df <-
    function(.data,
             .column = NULL,
             athena_table,
             athena_column,
             where_athena_col = NULL,
             where_athena_col_equals = NULL) {
        
        
                table_name <- paste0("v", stampede::stamp_this(without_punct = TRUE))
                
                if (is.null(dataframe_column)) {
                    
                    dataframe_column <- colnames(dataframe)[1]
                    
                }
                
                conn <- connect_athena()
                DatabaseConnector::dbWriteTable(conn = conn,
                                                name = table_name,
                                                value = dataframe)
                dc_athena(conn = conn)

            if (is.null(where_athena_col)) {
                    output <- query_athena(paste0("SELECT * FROM ", table_name, " LEFT JOIN ", athena_table, " c ON c.", athena_column, " = ", dataframe_column),
                                           cache_resultset = FALSE)
                        
            } else {
                    
                        output <- query_athena(paste0("SELECT * FROM ", table_name, " LEFT JOIN ", athena_table, " c ON c.", athena_column, " = ", dataframe_column, " WHERE c.", where_athena_col, " IN ", seagull::write_where_in_string(where_athena_col_equals)),
                                               cache_resultset = FALSE)
                        
                    
            }
                
            drop_left_join_tables()

        return(output)
    }