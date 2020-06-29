#' Iterate query_phrase function over a vector of phrases
#' @param phrases vector of phrases to iterate the query_phrase function on
#' @importFrom rubix map_names_set
#' @importFrom purrr map
#' @importFrom purrr set_names
#' @export

query_phrases <- 
        function(phrases, 
                 type = c("like", "exact"), 
                 limit = NULL, 
                 case_insensitive = TRUE,
                 set_names_to = NULL,
                 return_valid_only = TRUE)  
            {
            
                if (is.null(set_names)) {
                    
                        phrases %>%
                            rubix::map_names_set(function(x) query_phrase(x,
                                                                          limit = limit,
                                                                          type = type,
                                                                          case_insensitive = case_insensitive,
                                                                          return_valid_only = return_valid_only))
                    
                } else {
                    
                        phrases %>%
                            purrr::map(function(x) query_phrase(x,
                                                                          limit = limit,
                                                                          type = type,
                                                                          case_insensitive = case_insensitive,
                                                                return_valid_only = return_valid_only)) %>%
                            purrr::set_names(set_names_to)
                        
                }
        }

