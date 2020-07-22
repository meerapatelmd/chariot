#' Concert a Label Column to a Merge Column
#' @import dplyr
#' @import tidyr
#' @import rubix
#' @export

labelToMerge <-
        function(.data,
                 label_col,
                 into,
                 remove = FALSE) {

                label_col <- enquo(label_col)
                into <- enquo(into)

                .data %>%
                        tidyr::extract(col = !!label_col,
                                        into = c("concept_id", "concept_name"),
                                        regex = "(^.*?) (.*$)",
                                        remove = remove) %>%
                        rubix::mutate_to_integer(concept_id) %>%
                        dplyr::rename(label_concept_id = concept_id) %>%
                        left_join_concept(column = "label_concept_id",
                                          include_synonyms = FALSE)  %>%
                        merge_concepts(into = !!into)

        }
