#' Query montherapies in HemOnc
#' @importFrom rubix filter_at_grepl
#' @importFrom rubix arrange_by_nchar
#' @export


hemonc_monotherapy <-
    function(component) {
        
        output <-
            query_phrase(component, type = "like")
        
        output %>%
            rubix::filter_at_grepl(concept_name,
                                   grepl_phrase = "monotherapy") %>%
            rubix::arrange_by_nchar(nchar_col = concept_name) %>%
            filter_for_hemonc()
    }