#' Query Parent-Child Relationships from Concept Ancestor Table
#' @description This function takes all the ancestors and descendants with a level == 1 relationships (either max or min) for plotting.
#' @import dplyr
#' @export

left_join_for_children <- 
    function(.data, .col = NULL, override_cache = FALSE) {
        
        conn <- connect_athena()
        tables <- DatabaseConnector::dbListTables(conn = conn)
        dc_athena(conn)
        
        if (!("CONCEPT_PARENT" %in% tables)) {
            
                conn <- connect_athena()
            
                pre_output <- 
                            query_athena(sql_statement="SELECT DISTINCT ancestor_concept_id,descendant_concept_id FROM concept_ancestor WHERE min_levels_of_separation = '1' OR max_levels_of_separation = '1'", override_cache = TRUE) %>%
                            dplyr::select(parent_concept_id = ancestor_concept_id,
                                          child_concept_id = descendant_concept_id) %>%
                            dplyr::distinct() 
                    
                DatabaseConnector::dbWriteTable(conn = conn,
                                                name = "CONCEPT_PARENT",
                                                value = pre_output)
                
                DatabaseConnector::dbSendStatement(conn = conn,
                "ALTER TABLE concept_parent ADD CONSTRAINT xpk_concept_parent PRIMARY KEY (parent_concept_id,child_concept_id);
            CREATE INDEX idx_concept_parent_id_1  ON concept_parent  (parent_concept_id ASC);
            CLUSTER concept_parent  USING idx_concept_parent_id_1 ;
            CREATE INDEX idx_concept_parent_id_2 ON concept_parent (child_concept_id ASC);")
            
                
                DatabaseConnector::dbSendStatement(conn = conn,
                "ALTER TABLE concept_parent ADD CONSTRAINT fpk_concept_parent_concept_1 FOREIGN KEY (parent_concept_id)  REFERENCES concept (concept_id);
ALTER TABLE concept_parent ADD CONSTRAINT fpk_concept_parent_concept_2 FOREIGN KEY (child_concept_id)  REFERENCES concept (concept_id);")
                
                dc_athena(conn = conn)
                
            
        }
        
        left_join_df(.data = .data,
                     .col = .col,
                     athena_table = "concept_parent",
                     athena_column = "parent_concept_id")
        
    }