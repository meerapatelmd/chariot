#' Parse a Concept Label
#' @description Parse a concept Label in the format of "{concept_id} {concept_name}".
#' @import tidyr
#' @import dplyr
#' @export





parseLabel <-
        function(.data,
                 label_col,
                 remove = FALSE) {


                label_col <- enquo(label_col)


                .data %>%
                        tidyr::extract(col = !!label_col,
                                       into = c("concept_id", "concept_name"),
                                       regex = "(^.*?) (.*$)",
                                       remove = remove)

        }
