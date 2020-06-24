#' Query HemOnc regimens with 4 components
#' @importFrom centipede nchar_comma
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @export

query_4_hemonc_component_regs <-
    function(component) {
        output <- query_n_hemonc_component_regs(component)
        output %>%
            dplyr::mutate(comp_count = centipede::nchar_comma(concept_name)) %>%
            dplyr::filter(comp_count == 3) %>%
            dplyr::select(-comp_count)
    }