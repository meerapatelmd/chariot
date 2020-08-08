#' @title  Filter Columns with Merged Concept Strips
#' @description
#' This function filters a column that contains Concept Strips using Concept Table parameters. The target column may contain 1 or more merged concept strip, and the multiple strips must be separated by a new line \"\\n\" for the filter to operate correctly. It is important to note that the the filter is applied to the entire Concept Strip cell and will not alter the data content within the cell otherwise. For example, if the filter `vocabulary_id == 'RxNorm'` is used for `ColumnA`, a `ColumnA` cell that contains at least 1 RxNorm concept will be filtered for though there are other non-RxNorm concepts in that same cell.
#'
#' @param .data         dataframe with the merged concept column
#' @param merge_col     column of merged concepts
#' @param ...           arguments that will be passed to the dplyr filter function using the base Concept Table field names
#'
#' @return
#' A tibble with the same number of columns as the input with the number of rows equal or less than that of the input.
#'
#' @details
#' This function:
#' 1. Mutates a copy `merge_col` to a `merge_col_tmp` column,
#' 1. Separates the rows in `merge_col_tmp` by \"\\n\",
#' 1. Filters out any blanks and `NA` values introduced by the row separation,
#' 1. Unmerges the `merge_col` into the base Concept Table field names,
#' 1. Applies the filters found in the ellipses argument,
#' 1. Removes the `merge_col_tmp` and base Concept Table columns to reconstitute the original input, and
#' 1. Removes duplicate rows
#'
#' @seealso
#'  \code{\link[dplyr]{tidyeval-compat}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{filter_all}},\code{\link[dplyr]{filter}},\code{\link[dplyr]{select}},\code{\link[dplyr]{distinct}}
#'  \code{\link[rlang]{as_name}}
#'  \code{\link[tidyr]{separate_rows}}
#'  \code{\link[rubix]{normalize_all_to_na}}
#' @rdname filterStrip
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr enquo mutate filter_at filter select distinct
#' @importFrom rlang as_name
#' @importFrom tidyr separate_rows
#' @importFrom rubix normalize_all_to_na

filterStrip <-
    function(.data,
             merge_col,
             ...) {

            merge_col <- dplyr::enquo(merge_col)
            tmp_col <- paste0(rlang::as_name(merge_col), "tmp")


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

            .output <-
            .data %>%
                dplyr::mutate(!!tmp_col := !!merge_col) %>%
                separateConceptStrip(!!tmp_col) %>%
                # tidyr::separate_rows(!!tmp_col,
                #                      sep = "\n") %>%
                rubix::normalize_all_to_na() %>%
                dplyr::filter_at(vars(!!tmp_col), all_vars(!is.na(.))) %>%
                unmergeStrip(strip_col = !!tmp_col,
                             remove = FALSE) %>%
                dplyr::filter(...) %>%
                dplyr::select(-any_of(column_names)) %>%
                dplyr::select(-!!tmp_col) %>%
                dplyr::distinct()

            qa <- nrow(.output) > nrow(.data)

            if (qa) {
                    warning('returned data has more rows than input data')
            }

            return(.output)

    }
