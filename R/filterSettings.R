#' @title Filter Settings
#' @description
#'
#' @param .data PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{map_if}},\code{\link[purrr]{map2}}
#'  \code{\link[cave]{vector_to_string}}
#'  \code{\link[secretary]{typewrite_bold}},\code{\link[secretary]{typewrite}}
#'  \code{\link[rlang]{parse_expr}}
#' @rdname filterSettings
#' @export
#' @importFrom purrr map_if map2
#' @importFrom cave vector_to_string
#' @importFrom secretary typewrite_bold typewrite
#' @importFrom rlang parse_expr


filterSettings <-
        function(.data) {

                if (!exists(".Settings", envir = globalenv())) {

                        createSettings()

                }

                if (length(.Settings) == 0) {
                        stop('No .Settings to filter. See .Settings object')
                }


                settings <-
                        .Settings %>%
                        purrr::map_if(function(x) !(length(x) == 1 && is.na(x)),
                                      function(x) cave::vector_to_string(x)) %>%
                        purrr::map_if(function(x) (length(x) == 1 && is.na(x)),
                                      function(x) paste0("c(", x, ")")) %>%
                        purrr::map2(names(.Settings),
                                    function(x,y) paste("\t", y,"%in%", x, collapse = " ")) %>%
                        unlist() %>%
                        unname() %>%
                        paste(collapse = ",\n")

                secretary::typewrite_bold("Settings:")
                secretary::typewrite(settings)

                cat("\n")

                eval(rlang::parse_expr(paste0(".data %>% dplyr::filter(", settings, ")")))

        }
