#' Filter Merged Concepts
#' @param .data dataframe that contains the merged concept column
#' @param merge_col column of merged concepts
#' @param ... arguments for filter function using concept table fields
#' @importFrom dplyr enquo
#' @importFrom dplyr filter
#' @export


filterStrip <-
    function(.data,
             merge_col,
             ...) {


            merge_col <- dplyr::enquo(merge_col)


            column_names <-  c("concept_id",
                                      "concept_name",
                                      "domain_id",
                                      "vocabulary_id",
                                      "concept_class_id",
                                      "standard_concept",
                                      "concept_code",
                                      "valid_start_date",
                                      "valid_end_date",
                                      "invalid_reason")


            if (any(column_names %in% colnames(.data))) {

                    qa <- column_names[column_names %in% colnames(.data)]

                    stop('data cannot have any concept table column names: ', paste(qa, collapse = ", "))

            }

            .data %>%
                unmergeStrip(strip_col = !!merge_col,
                             remove = FALSE) %>%
                dplyr::filter(...) %>%
                dplyr::select(-any_of(column_names))
    }
