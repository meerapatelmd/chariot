#' Query a vector of permissible values in bulk
#' @description This function queries the CONCEPT and CONCEPT_SYNONYM table for a vector of permissible values, or valueset,
#' @param valueset vector of permissible values to search
#' @param type the type of query to perform. Defaults to "exact".
#' @importFrom purrr map
#' @importFrom purrr set_names
#' @importFrom purrr transpose
#' @export

bulk_query_valueset <-
    function(valueset, type = "exact") {
        syns <-
            valueset %>%
            purrr::map(function(x) query_phrase_synonym(x, type = type)) %>%
            purrr::set_names(valueset)
        
        cons <- 
            valueset %>%
            purrr::map(function(x) query_phrase(x, type = type)) %>%
            purrr::set_names(valueset)
        
        search_results <-
            list(CONCEPT = cons,
                 CONCEPT_SYNONYM = syns) %>% 
            purrr::transpose() 
        
        for (i in 1:length(search_results)) {
            search_result <- search_results[[i]]
            search_result_concept <- search_result$CONCEPT
            search_result_synonym <- search_result$CONCEPT_SYNONYM
            search_results[[i]]$CONCEPT_SYNONYM <- search_result_synonym[!(search_result_synonym$concept_id %in% search_result_concept$concept_id),]
        }
        
        return(search_results)
        
    }