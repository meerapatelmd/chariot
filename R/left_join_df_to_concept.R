#' Join a dataframe object with Athena's concept table
#' @description This function has better performance than the WHERE IN statement for larger searches. A table in the format of "v{unpunctuated timestamp}" is written to the local Athena. A join is performed with a concept table. The table is subsequently dropped. If a dataframe column is not provided as a the column to join the concept table on, the 1st column will be used by default.
#' @param dataframe dataframe to join
#' @param dataframe_column string of the column name to join on. If NULL, the 1st column is used.
#' @param concept_column name of concept_column to join dataframe on. Defaults to concept ID.
#' @importFrom seagull create_table_via_temp_file
#' @importFrom seagull drop_table
#' @export

left_join_df_to_concept <-
    function(dataframe,
             dataframe_column = NULL,
             concept_column = "concept_id") {
        
        table_name <- paste0("v", stampede::stamp_this(without_punct = TRUE))
        
        if (is.null(dataframe_column)) {
            dataframe_column <- colnames(dataframe)[1]
        }
        
        seagull::create_table_via_temp_file(dataframe = dataframe,
                                            table_name = table_name,
                                            dbname = "athena")
        
        output <- query_athena(paste0("SELECT * FROM ", table_name, " LEFT JOIN concept c ON c.", concept_column, " = ", dataframe_column))
        
        
        seagull::drop_table(table_name = table_name,
                            dbname = "athena")
        
        
        return(output)
    }