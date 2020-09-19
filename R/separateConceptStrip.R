#' @title Separate Concept Strips by Row
#' @description
#' This function separates a merged Concept Strip Column into rows by new line \"\\n\".
#' @param .data A data frame.
#' @param ... Columns to separate across multiple rows that are passed to \code{\link[tidyr]{separate_rows}}.
#' @return
#' A longer tibble if the merged Concept Strip Column/s have greater than 1 Concept Strips.
#' @seealso
#'  \code{\link[tidyr]{separate_rows}}
#' @rdname separateConceptStrip
#' @export
#' @importFrom tidyr separate_rows

separateConceptStrip <-
        function(.data,
                 ...) {

                tidyr::separate_rows(.data,
                                     ...,
                                     sep = "(?<=\\])\n(?=\\[A-Z\\])")
        }
