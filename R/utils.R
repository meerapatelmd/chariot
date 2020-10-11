#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL



#' @noRd

normalize_nas <-
        function (data, blanks = TRUE)
        {

                data[data %in% c("NA", "")] <- NA_character_
                data

        }

#' @noRd

make_temp_table_name <-
        function() {

                paste0("V", stringr::str_remove_all(as.character(Sys.time()), "[[:punct:]]|\\s"))
        }




