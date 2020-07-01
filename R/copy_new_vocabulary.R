#' Copy New Vocabulary to Athena
#' @import seagull
#' @import secretary
#' @import police
#' @import DatabaseConnector
#' @export


copy_new_vocabulary <- 
    function(folder_path = "~/Desktop/vocabulary_download_v5_{a6b9f6b7-f8ef-4d04-ac57-6cdb7f17fc31}_1593572935845") {
        
        if (!dir.exists(folder_path)) {
            
            stop('dir "', folder_path, '" does not exist.')
            
        }
        
        vocabulary_files <-
            list.files(folder_path,
                       full.names = TRUE,
                       pattern = "[.]csv$")
        
        if (any(grepl("CONCEPT_CPT4.csv", vocabulary_files))) {
            
            file.remove(grep("CONCEPT_CPT4.csv", vocabulary_files, value = TRUE))
            vocabulary_files <-
                list.files(folder_path,
                           full.names = TRUE,
                           pattern = "[.]csv$")
        }
        
        table_names <- cave::strip_fn(vocabulary_files)
        
        conn <- seagull::connect_to_local_postgres(dbname = "athena")
        
        while (length(vocabulary_files) > 0) {
            
            vocabulary_file <- vocabulary_files[1]
            table_name <- table_names[1]
            
            sql <- paste0("COPY ", table_name, " FROM '", vocabulary_file, "' WITH DELIMITER E'\t' CSV HEADER QUOTE E'\b' ;")
            
            secretary::typewrite("Starting", table_name, "...")
            
            
            police::try_catch_error_as_null(
                DatabaseConnector::dbSendStatement(conn = conn,
                                                   statement = sql))
            
            
            secretary::typewrite("Completed", table_name)
            
            file.remove(vocabulary_file)
            vocabulary_files <- vocabulary_files[-1]
            table_names <- table_names[-1]
            
        }
        
        
        DatabaseConnector::dbDisconnect(conn = conn)
        
        
    }