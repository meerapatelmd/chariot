#' Join a dataframe object with Athena's concept table
#' @description This function has better performance than the WHERE IN statement for larger searches. A table in the format of "v{unpunctuated timestamp}" is written to the local Athena. A join is performed with a concept table. The table is subsequently dropped. If a dataframe column is not provided as a the column to join the concept table on, the 1st column will be used by default.
#' @param dataframe dataframe to join
#' @param dataframe_column string of the column name to join on. If NULL, the 1st column is used.
#' @param athena_column name of column to join dataframe on. Defaults to concept ID.
#' @importFrom seagull create_table_via_temp_file
#' @importFrom seagull drop_table
#' @export

left_join_df <-
    function(dataframe,
             dataframe_column = NULL,
             athena_table,
             athena_column) {
        
        table_name <- paste0("v", stampede::stamp_this(without_punct = TRUE))
        
        if (is.null(dataframe_column)) {
            dataframe_column <- colnames(dataframe)[1]
            
        }
        
        #Loading cache
        output <-
            load_cached_join(function_name = "left_join_df",
                             left_vector = dataframe,
                             right_table_name = athena_table,
                             right_column_name = athena_column)
        
        
        if (is.null(output)) { 
                
            seagull::create_table_via_temp_file(dataframe = dataframe,
                                                table_name = table_name,
                                                dbname = "athena")
            
            output <- query_athena(paste0("SELECT * FROM ", table_name, " LEFT JOIN ", athena_table, " c ON c.", athena_column, " = ", dataframe_column))
            
            
            seagull::drop_table(table_name = table_name,
                                dbname = "athena")
            
            cache_join(function_name = "left_join_df",
                             left_vector = dataframe,
                             right_table_name = athena_table,
                             right_column_name = athena_column,
                        object = output)
            
        }
        
        return(output)
    }