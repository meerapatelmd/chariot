#' Query all HemOnc regimens with more than 2 components
#' @importFrom rubix filter_at_grepl
#' @importFrom rubix arrange_by_nchar
#' @export

hemonc_regimens <-
    function(component, 
             component_count = NULL,
             omop = FALSE,
             omop_schema = "omop_vocabulary") {
        
        output <-
                query_phrase(component, 
                             type = "like",
                             where_col = "vocabulary_id",
                             where_col_in = "HemOnc",
                             omop = omop,
                             omop_schema = omop_schema)
        
        output <-
        output %>%
            rubix::filter_at_grepl(concept_name,
                                   grepl_phrase = ", | and | monotherapy") %>%
            rubix::arrange_by_nchar(nchar_col = concept_name) 
        
        if (is.null(component_count)) {
                return(output)
        } else if (component_count == 1) {
            
            output <-
                output %>%
                rubix::filter_at_grepl(concept_name,
                                       grepl_phrase = " monotherapy") %>%
                rubix::arrange_by_nchar(nchar_col = concept_name) 
            
            return(output)
            
        } else if (component_count == 2) {
            
                output %>%
                    rubix::filter_at_grepl(concept_name,
                                           grepl_phrase = " and ") %>%
                rubix::arrange_by_nchar(nchar_col = concept_name) 
        } else if (component_count == 3) {
            
            
                output %>%
                        dplyr::mutate(comma_count = centipede::nchar_comma(concept_name)) %>%
                        dplyr::filter(comma_count == 2) %>%
                rubix::arrange_by_nchar(nchar_col = concept_name) 
            
        } else if (component_count == 4) {
            
                output %>%
                    dplyr::mutate(comma_count = centipede::nchar_comma(concept_name)) %>%
                    dplyr::filter(comma_count == 3) %>%
                    rubix::arrange_by_nchar(nchar_col = concept_name) 
            
        } else {
            max <-  1 + (output %>%
                        dplyr::transmute(comma_count = centipede::nchar_comma(concept_name)) %>%
                        unlist() %>%
                        max(na.rm = TRUE))
            warning('"component_count" max is: ', max,  ' returning unfiltered output')
            return(output)
            
        }
    }

