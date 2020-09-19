#' Open concept by concept_id in Browser
#' @export


browseAthena <-
    function(concept_id) {
        browseURL(url = paste0("http://athena.ohdsi.org/search-terms/terms/", concept_id))
    }

