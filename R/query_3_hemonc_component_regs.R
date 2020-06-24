#' Query HemOnc regimens with 3 components
#' @importFrom centipede nchar_comma
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @export

query_3_hemonc_component_regs <-
    function(component) {
        output <- query_n_hemonc_component_regs(component)
        output %>%
            dplyr::mutate(comp_count = centipede::nchar_comma(concept_name)) %>%
            dplyr::filter(comp_count == 2) %>%
            dplyr::select(-comp_count)
    }