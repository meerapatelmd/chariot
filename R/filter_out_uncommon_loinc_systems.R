#' Filter out uncommon LOINC Systems
#' @importFrom dplyr enquo
#' @importFrom rubix filter_at_grepl
#' @export

return_uncommon_loinc_systems <-
    function(dataframe, loinc_systems_col) {
        
            loinc_systems_col <- dplyr::enquo(loinc_systems_col)
            dataframe %>%
            rubix::filter_at_grepl(col = !!loinc_systems_col,
                                   grepl_phrase = "Donor|XX|Cerebra|Peric|Body|Perito|Platelet poor|Stool|Sediment|Gastric|Synovial|Lymph Node|Pleural",
                                   evaluates_to = FALSE)
        
    }