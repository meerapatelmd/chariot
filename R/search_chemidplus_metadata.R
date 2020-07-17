#' #' Get metadata on a phrase
#' #' @description Get metadata
#' #' @import httr
#' #' @import rvest
#' #' @import stringr
#' #' @import police
#' #' @import centipede
#' #' @export
#' 
#' search_chemidplus_metadata <-
#'     function(
#'              type = "contains",
#'              conn) {
#'         
#'             destSchema <- "chemidplus"
#'             urlTable <- "url"
#'             destTable <- "metadata"
#' 
#'             input <- 
#'                 DatabaseConnector::dbGetQuery(conn = conn,
#'                                               statement = "SELECT * FROM chemidplus.url") %>%
#'                 as_tibble() %>%
#'                 rubix::normalize_all_to_na() %>%
#'                 tibble::rowid_to_column() %>%
#'                 dplyr::filter(is.na(metadata_status)) 
#'             
#'             
#'             url0s <- input$url0
#'             rowids <- input$rowid
#'             
#'             output <- list()
#'             for (i in 1:length(url0s)) {
#'                     
#'                     url <- police::try_catch_error_as_null(url(url0s[i], "rb"))
#'                     Sys.sleep(5)
#'                     resp <- police::try_catch_error_as_null(read_html(url))
#'                     
#'                     #close(url)
#'                     
#'                     if (!is.null(url)) {
#'                         
#'                         resp <- police::try_catch_error_as_null(read_html(url))
#'                         
#'                         Sys.sleep(5)
#'                         
#'                         
#'                         close(url)
#'                         
#'                         if (!is.null(resp)) {
#'                             # If there aren't any #names headers, it is likely that the query resulted in multiple search results and needs to be tied to an RN number
#'                             
#'                             qa <-
#'                                 resp %>%
#'                                 html_nodes("#names")
#'                             
#'                             # If there are 0 html_names #names, checking to see if it landed on a multiple results page
#'                             if (length(qa) == 0) {
#'                                 #print("733")
#'                                 
#'                                 multiple_results <-
#'                                     resp %>%
#'                                     html_nodes("div") %>%
#'                                     html_text() %>%
#'                                     rubix::vector_to_tibble(new_col = "multiple_results") %>%
#'                                     mutate_all(trimws) %>%
#'                                     rubix::filter_at_grepl(multiple_results,
#'                                                            grepl_phrase = "[0-9]{2,}[-]{1}[0-9]{2,}[-]{1}[0-9]{1,}$") %>%
#'                                     tidyr::extract(col = multiple_results,
#'                                                    into = c("chemidplus_name", "chemidplus_rn"),
#'                                                    regex = "(^.*?)([0-9]{2,}[-]{1}[0-9]{2,}[-]{1}[0-9]{1,}$)") %>%
#'                                     dplyr::mutate(source_term = phrase) %>%
#'                                     dplyr::mutate(chemidplus_rn_url = paste0("https://chem.nlm.nih.gov/chemidplus/rn/", chemidplus_rn))
#'                                 
#'                                 print(multiple_results)
#'                                 
#'                                 output[[i]] <- multiple_results
#'                                 namess(output)[[i]] <- rowids[i]
#'                 
#'                 
#'                             }
#'                         }
#'                     }
#'             }
#'     }
#'             
#'                     
#' 
#'             if (!is.null(url)) {
#' 
#'                         resp <- police::try_catch_error_as_null(read_html(url))
#'                         
#'                         Sys.sleep(5)
#'                         
#'                         close(url)
#'             
#'                         if (!is.null(resp)) {
#'                         # If there aren't any #names headers, it is likely that the query resulted in multiple search results and needs to be tied to an RN number
#'                             
#'                                 qa <-
#'                                     resp %>%
#'                                     html_nodes("#names")
#'                                 
#'                                 # If there are 0 html_names #names, checking to see if it landed on a multiple results page
#'                                 if (length(qa) == 0) {
#'                     #print("733")
#'                     
#'                                         multiple_results <-
#'                                                 resp %>%
#'                                                 html_nodes("div") %>%
#'                                                 html_text() %>%
#'                                                 rubix::vector_to_tibble(new_col = "multiple_results") %>%
#'                                                 mutate_all(trimws) %>%
#'                                                 rubix::filter_at_grepl(multiple_results,
#'                                                                        grepl_phrase = "[0-9]{2,}[-]{1}[0-9]{2,}[-]{1}[0-9]{1,}$") %>%
#'                                                 tidyr::extract(col = multiple_results,
#'                                                                into = c("chemidplus_name", "chemidplus_rn"),
#'                                                                regex = "(^.*?)([0-9]{2,}[-]{1}[0-9]{2,}[-]{1}[0-9]{1,}$)") %>%
#'                                                 dplyr::mutate(source_term = phrase) %>%
#'                                                 dplyr::mutate(chemidplus_rn_url = paste0("https://chem.nlm.nih.gov/chemidplus/rn/", chemidplus_rn))
#'                                         
#'                                         multiple_results_metadata <- multiple_results
#'                                     #     if (nrow(multiple_results)) {
#'                                     #         
#'                                     #             rm(resp)
#'                                     #             resp2 <- list()
#'                                     #             
#'                                     #             while (nrow(multiple_results) > 0) {
#'                                     #                 
#'                                     #                     single_result <- 
#'                                     #                         multiple_results %>%
#'                                     #                         rubix::filter_first_row()
#'                                     #                     
#'                                     #                     
#'                                     #                     url <- police::try_catch_error_as_null(url(single_result$chemidplus_rn_url, "rb"))
#'                                     #                     Sys.sleep(5)
#'                                     #                     
#'                                     #                     resp2[[length(resp2)+1]] <- police::try_catch_error_as_null(read_html(url))
#'                                     #                     
#'                                     #                     Sys.sleep(5)
#'                                     #                     
#'                                     #                     if (!is.null(url)) {
#'                                     #                         
#'                                     #                             close(url)
#'                                     #                             Sys.sleep(5)
#'                                     #                         
#'                                     #                     }
#'                                     #                     
#'                                     #                     
#'                                     #                     
#'                                     #                     multiple_results <- 
#'                                     #                         multiple_results %>%
#'                                     #                             rubix::filter_first_row(invert = TRUE)
#'                                     #                 
#'                                     #             }
#'                                     #         
#'                                     #     }
#'                                     #     
#'                                     # resp2 <- 
#'                                     #     resp2 %>%
#'                                     #     purrr::map(function(x) x %>%
#'                                     #                                html_nodes("#names") %>%
#'                                     #                                html_text() %>%
#'                                     #                                strsplit(split = "\n") %>%
#'                                     #                                unlist() %>%
#'                                     #                                stringr::str_remove_all("Systematic Name|Names and Synonyms|Results Name|Name of Substance|MeSH Heading|Synonyms|[^ -~]") %>%
#'                                     #                                trimws("both") %>%
#'                                     #                                centipede::no_blank() %>%
#'                                     #                                unique())
#'                                     # 
#'                                     # resp <- resp2
#'                                     
#'                                     final <- multiple_results_metadata 
#'                             
#'                         # For single page responses
#'                         } else {
#'                             
#'                                     single_results <- 
#'                                                 tibble(source_term = phrase,
#'                                                        chemidplus_name_url = url0)
#'                                                        
#'                             
#'                                     # resp <-
#'                                     #         resp %>%
#'                                     #         html_nodes("#names") %>%
#'                                     #         html_text() %>%
#'                                     #         strsplit(split = "\n") %>%
#'                                     #         unlist() %>%
#'                                     #         stringr::str_remove_all("Systematic Name|Names and Synonyms|Results Name|Name of Substance|MeSH Heading|Synonyms|[^ -~]") %>%
#'                                     #         trimws("both") %>%
#'                                     #         centipede::no_blank() %>%
#'                                     #         unique()
#'                                     # 
#'                                     
#'                                     final <- single_results
#' 
#'                         }
#' 
#'                                 Sys.sleep(5)
#'                                 closeAllConnections()
#'                                 
#'                         } else {
#'                             
#'                             final <- 
#'                                 tibble(source_term = phrase,
#'                                        chemidplus_name_url = url0,
#'                                        chemidplus_name_url_status = "valid",
#'                                        response_status = "NULL") 
#'                             
#'                         }
#'                         
#'             } else {
#'                 
#'                     final <- 
#'                         tibble(source_term = phrase,
#'                                chemidplus_name_url = url0,
#'                                chemidplus_name_url_status = "invalid")
#'                 
#'                 
#'             }
#' 
#'             
#'             #secretary::press_enter()
#'             
#'             sql <- SqlRender::render(SqlRender::readSql("inst/sql/loadChemiDPlusMetadata.sql"), destSchema = destSchema)
#'             
#'             DatabaseConnector::executeSql(connection = conn, 
#'                                           sql, 
#'                                           progressBar = TRUE, 
#'                                           reportOverallTime = TRUE)
#'             
#'             tablename <- paste0(destSchema, ".", destTable)
#'             tablename_dated <- paste0(tablename, "_", stringr::str_remove_all(as.character(Sys.time()), "[[:punct:]]| "))
#'             
#'             tablename_exists <<- toupper(destTable) %in% toupper((DatabaseConnector::dbListTables(conn = conn, schema = destSchema)))
#'             if (!tablename_exists) {
#' 
#'                 DatabaseConnector::dbWriteTable(conn = conn,
#'                                                 name = tablename,
#'                                                 value = final %>%
#'                                                     as.data.frame())
#'                 
#'                 
#'                 DatabaseConnector::dbWriteTable(conn = conn,
#'                                                 name = tablename_dated,
#'                                                 value = final %>%
#'                                                     as.data.frame())
#'                 
#'             } else {
#'                 
#'                 current_metadata <- DatabaseConnector::dbGetQuery(conn = conn,
#'                                                                   paste0("SELECT * FROM ", tablename))
#'                 
#'                 final <- 
#'                     dplyr::bind_rows(current_metadata,
#'                                      final) %>%
#'                     dplyr::distinct()
#'                 
#'                 DatabaseConnector::dbRemoveTable(conn = conn,
#'                                                  name = tablename)
#'                 
#'                 DatabaseConnector::dbWriteTable(conn = conn,
#'                                                 name = tablename,
#'                                                 value = final %>%
#'                                                     as.data.frame())
#'                 
#'                 
#'                 DatabaseConnector::dbWriteTable(conn = conn,
#'                                                 name = tablename_dated,
#'                                                 value = final %>%
#'                                                     as.data.frame())
#'                 
#'             }
#'     }