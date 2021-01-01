#' @title Create a Search Settings Object
#' @description
#' This function takes the Concept Table fields present in the global environment and aggregates them into a list object that can then be run against a data frame as a whole using the \code{\link{filterSettings}} function.
#'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @seealso
#'  \code{\link[rubix]{map_names_set}}
#'  \code{\link[purrr]{keep}},\code{\link[purrr]{map}}
#' @rdname set_search_parameters
#' @export
#' @importFrom magrittr %>%
#' @importFrom rubix map_names_set
#' @importFrom purrr keep map


set_search_parameters <-
  function(obj = "SEARCH_SETTINGS",
           rm_source_obj = TRUE) {
    parameters <-
      c(
        "concept_code",
        "vocabulary_id",
        "domain_id",
        "concept_class_id",
        "standard_concept",
        "invalid_reason"
      )


    settings <-
      parameters %>%
      rubix::map_names_set(function(x) {
        if (exists(x, envir = parent.frame())) {
          get(x, envir = parent.frame())
        }
      }) %>%
      purrr::keep(~ !is.null(.)) %>%
      purrr::map(~ ifelse(is.na(.), NA_character_, .))



    if (rm_source_obj) {
      parameters %>%
        purrr::map(function(x) {
          if (exists(x, envir = parent.frame())) {
            rm(list = x, envir = parent.frame())
          }
        })
    }

    assign(obj, value = settings, envir = parent.frame())
  }


#' @title Filter Settings
#' @description
#'
#' @param .data PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
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
      stop("No .Settings to filter. See .Settings object")
    }


    settings <-
      .Settings %>%
      purrr::map_if(
        function(x) !(length(x) == 1 && is.na(x)),
        function(x) cave::vector_to_string(x)
      ) %>%
      purrr::map_if(
        function(x) (length(x) == 1 && is.na(x)),
        function(x) paste0("c(", x, ")")
      ) %>%
      purrr::map2(
        names(.Settings),
        function(x, y) paste("\t", y, "%in%", x, collapse = " ")
      ) %>%
      unlist() %>%
      unname() %>%
      paste(collapse = ",\n")

    secretary::typewrite_bold("Settings:")
    secretary::typewrite(settings)

    cat("\n")

    eval(rlang::parse_expr(paste0(".data %>% dplyr::filter(", settings, ")")))
  }
