#' Convert a Merge Strip to a Label
#' @import dplyr
#' @export

mergeToLabel <-
        function(.data,
                 merge_col,
                 into,
                 remove = FALSE) {

                merge_col <- enquo(merge_col)
                into <- enquo(into)

                unmerge_concepts(dataframe = .data,
                                          concept_col = !!merge_col,
                                          remove = remove) %>%
                        makeLabel(into = !!into,
                                  remove = remove)
        }
