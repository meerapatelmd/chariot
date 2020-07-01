#' Query all HemOnc regimens with more than 2 components
#' @importFrom rubix filter_at_grepl
#' @importFrom rubix arrange_by_nchar
#' @export

query_hemonc_regimens <-
    function(component) {
        
        output <-
            query_phrase(component, type = "like")
        
        output %>%
            rubix::filter_at_grepl(concept_name,
                                   grepl_phrase = ", ") %>%
            rubix::arrange_by_nchar(nchar_col = concept_name) %>%
            filter_for_hemonc()
    }