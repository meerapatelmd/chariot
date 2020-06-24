#' Locates the vocabulary table and search type where the phrase is first found
#' @description Useful if a simple search isn't returning results.
#' @export

locate_phrase <- 
    function(phrase) {
        
                exact_results <- query_phrase(phrase = phrase,
                                              type = "exact",
                                              limit = 1)
                
                if (nrow(exact_results) == 0) {
                        like_results <- 
                                query_phrase(phrase = phrase,
                                             type = "like",
                                             limit = 1)
                        
                        if (nrow(like_results) == 0) {
                            
                                synonym_exact_results <-
                                    query_phrase_synonym(phrase = phrase,
                                                         limit = 1,
                                                         type = "exact")
                                
                                if (nrow(synonym_exact_results) == 0) {
                                    
                                        synonym_like_results <-
                                            query_phrase_synonym(phrase = phrase,
                                                                 limit = 1,
                                                                 type = "like")
                                        
                                        if (nrow(synonym_like_results) == 0) {
                                            
                                                return("Not Found")
                                            
                                        } else {
                                            
                                                return("Synonym Like")
                                            
                                        }
                                    
                                } else {
                                    
                                        return("Synonym Exact")
                                    
                                }
                            
                            
                        } else {
                            
                            
                                return("Like")
                            
                        }
                    
                    
                } else {
                    
                        return("Exact")
                    
                }
        
    }