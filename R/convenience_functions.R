#' @title
#' Convenience Functions
#'
#' @description
#' Convenience functions that simply tasks by allowing them to be executed from the R console such as opening the OMOP Concept page.
#'
#' @keywords internal
NULL


#' Open concept by concept_id in Browser
#' @export


browse_athena <-
        function(concept_id) {
                browseURL(url = paste0("http://athena.ohdsi.org/search-terms/terms/", concept_id))
        }
