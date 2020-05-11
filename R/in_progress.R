





left_join_df_to_relationship <-
    function(dataframe,
             dataframe_column = NULL) {
        
        
        resultset <-
        left_join_df(dataframe = dataframe,
                     dataframe_column = dataframe_column,
                     athena_table = "concept_relationship",
                     athena_column = "concept_id_1")
        
        return(resultset)
        
    }

left_join_df_c2_to_concept <-
    function(dataframe,
             dataframe_column = "concept_id_2") {
        
        resultset <-
            left_join_df_to_concept(dataframe = dataframe,
                                    dataframe_column = dataframe_column) %>%
            dplyr::rename_at(vars(!one_of(dataframe_column)), function(x) paste(x, "_2"))
    }

