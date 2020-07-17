#' Find a ChemiDPlus Registry Number
#' @description Find the Registry Number/s (rn) for a drug
#' @import httr
#' @import rvest
#' @import stringr
#' @import police
#' @import centipede
#' @export

search_chemidplus_rn <-
    function(phrase,
             type = "contains") {

            #Remove all spaces
            phrase <- stringr::str_remove_all(phrase, "\\s")
        
            url0 <- paste0("https://chem.nlm.nih.gov/chemidplus/name/", type, "/",  phrase)
            url <- police::try_catch_error_as_null(url(url0, "rb"))
        
            Sys.sleep(5)

            if (!is.null(url)) {

                        resp <- police::try_catch_error_as_null(read_html(url))
                        
                        Sys.sleep(5)
                        
                        close(url)
            
                        if (!is.null(resp)) {
                        # If there aren't any #names headers, it is likely that the query resulted in multiple search results and needs to be tied to an RN number
                            
                                qa <-
                                    resp %>%
                                    html_nodes("#names")
                                
                                # If there are 0 html_names #names, checking to see if it landed on a multiple results page
                                if (length(qa) == 0) {
                    #print("733")
                    
                                        multiple_results <-
                                                resp %>%
                                                html_nodes("div") %>%
                                                html_text() %>%
                                                rubix::vector_to_tibble(new_col = "multiple_results") %>%
                                                mutate_all(trimws) %>%
                                                rubix::filter_at_grepl(multiple_results,
                                                                       grepl_phrase = "[0-9]{2,}[-]{1}[0-9]{2,}[-]{1}[0-9]{1,}$") %>%
                                                tidyr::extract(col = multiple_results,
                                                               into = c("chemidplus_name", "chemidplus_rn"),
                                                               regex = "(^.*?)([0-9]{2,}[-]{1}[0-9]{2,}[-]{1}[0-9]{1,}$)") %>%
                                                dplyr::mutate(source_term = phrase) %>%
                                                dplyr::mutate(chemidplus_rn_url = paste0("https://chem.nlm.nih.gov/chemidplus/rn/", chemidplus_rn))
                                        multiple_results_metadata <- multiple_results
                                        if (nrow(multiple_results)) {
                                            
                                                rm(resp)
                                                resp2 <- list()
                                                
                                                while (nrow(multiple_results) > 0) {
                                                    
                                                        single_result <- 
                                                            multiple_results %>%
                                                            rubix::filter_first_row()
                                                        
                                                        
                                                        url <- police::try_catch_error_as_null(url(single_result$chemidplus_rn_url, "rb"))
                                                        Sys.sleep(5)
                                                        
                                                        resp2[[length(resp2)+1]] <- police::try_catch_error_as_null(read_html(url))
                                                        
                                                        Sys.sleep(5)
                                                        
                                                        if (!is.null(url)) {
                                                            
                                                                close(url)
                                                                Sys.sleep(5)
                                                            
                                                        }
                                                        
                                                        
                                                        
                                                        multiple_results <- 
                                                            multiple_results %>%
                                                                rubix::filter_first_row(invert = TRUE)
                                                    
                                                }
                                            
                                        }
                                        
                                    resp2 <- 
                                        resp2 %>%
                                        purrr::map(function(x) x %>%
                                                                   html_nodes("#names") %>%
                                                                   html_text() %>%
                                                                   strsplit(split = "\n") %>%
                                                                   unlist() %>%
                                                                   stringr::str_remove_all("Systematic Name|Names and Synonyms|Results Name|Name of Substance|MeSH Heading|Synonyms|[^ -~]") %>%
                                                                   trimws("both") %>%
                                                                   centipede::no_blank() %>%
                                                                   unique())
                                    
                                    resp <- resp2
                                    
                                    final <- list(metadata = multiple_results_metadata,
                                                  response = resp)
                            
                        # For single page responses
                        } else {
                            
                                    single_results <- 
                                                tibble(source_term = phrase,
                                                       chemidplus_name_url = url0)
                                                       
                            
                            
                                    resp <-
                                            resp %>%
                                            html_nodes("#names") %>%
                                            html_text() %>%
                                            strsplit(split = "\n") %>%
                                            unlist() %>%
                                            stringr::str_remove_all("Systematic Name|Names and Synonyms|Results Name|Name of Substance|MeSH Heading|Synonyms|[^ -~]") %>%
                                            trimws("both") %>%
                                            centipede::no_blank() %>%
                                            unique()
                                    
                                    
                                    final <- list(metadata = single_results,
                                                  response = resp)

                        }
                                
                                return(final)

                                Sys.sleep(3)
                                closeAllConnections()
                        }
            }
    }