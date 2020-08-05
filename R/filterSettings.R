#' Filter Concept Table for Settings
#' @import purrr
#' @importFrom rlang parse_expr
#' @importFrom cave vector_to_string
#' @importFrom magrittr %>%
#' @export

filterSettings <-
        function(.data) {

                if (!exists("Settings", envir = globalenv())) {

                        stop("Settings object not found. Run createSettings function.")

                }



                settings <-
                        Settings %>%
                        purrr::map_if(function(x) !(length(x) == 1 && is.na(x)),
                                      function(x) cave::vector_to_string(x)) %>%
                        purrr::map_if(function(x) (length(x) == 1 && is.na(x)),
                                      function(x) paste0("c(", x, ")")) %>%
                        purrr::map2(names(Settings),
                                    function(x,y) paste("\t", y,"%in%", x, collapse = " ")) %>%
                        unlist() %>%
                        unname() %>%
                        paste(collapse = ",\n")

                secretary::typewrite_bold("Settings:")
                secretary::typewrite(settings)

                cat("\n")

                eval(rlang::parse_expr(paste0(".data %>% dplyr::filter(", settings, ")")))

        }
