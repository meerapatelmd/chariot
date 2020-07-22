#' Make Label Column
#' @description A Label is in the format of "{concept_id} concept_name". It is less comprehensive than a merged strip using the merge_concepts function, but more human readable when interfacing with others.
#' @importFrom tidyr unite
#' @import dplyr
#' @export

makeLabel <-
        function(.data,
                 into,
                 remove = FALSE) {

                into <- enquo(into)

                .data %>%
                        tidyr::unite(col = !!into,
                                     contains("concept_id"),
                                     contains("concept_name"),
                                     sep = " ",
                                     remove = remove)
        }
