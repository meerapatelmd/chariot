#' Iterate query_phrase function over a vector of phrases
#' @param phrases vector of phrases to iterate the query_phrase function on
#' @importFrom rubix map_names_set
#' @importFrom purrr map
#' @importFrom purrr set_names
#' @export

query_phrases <- 
        function(phrases, 
                 limit = NULL, 
                 type = c("like", "exact"), 
                 case_insensitive = TRUE,
                 set_names_to = NULL)  
            {
            
                if (is.null(set_names)) {
                    
                        phrases %>%
                            rubix::map_names_set(function(x) query_phrase(x,
                                                                          limit = limit,
                                                                          type = type,
                                                                          case_insensitive = case_insensitive))
                    
                } else {
                    
                        phrases %>%
                            purrr::map(function(x) query_phrase(x,
                                                                          limit = limit,
                                                                          type = type,
                                                                          case_insensitive = case_insensitive)) %>%
                            purrr::set_names(set_names_to)
                        
                }
        }

