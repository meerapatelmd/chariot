#' Extract HemOnc Regimens by Component String Match
#' @description This function uses the grepl function on the HemOnc Concept Names and naming patterns to return HemOnc Regimens based on a single Component and total number of Components in the source regimen.
#' @param component_count If NULL or the component_count is larger than the maximum number of components possible for all Regimens with a positive string match to the `component` parameter, the unfiltered result of the initial query for `component` is returned.
#' @importFrom rubix filter_at_grepl
#' @importFrom rubix arrange_by_nchar
#' @export

extractHemOncRegimens <-
    function(component,
             component_count = NULL,
             omop = FALSE,
             omop_schema = "omop_vocabulary") {

        output <-
                query_phrase(component,
                             type = "like",
                             where_col = "vocabulary_id",
                             where_col_in = "HemOnc",
                             omop = omop,
                             omop_schema = omop_schema)

        output <-
        output %>%
            rubix::filter_at_grepl(concept_name,
                                   grepl_phrase = ", | and | monotherapy") %>%
            rubix::arrange_by_nchar(nchar_col = concept_name)

        if (is.null(component_count)) {
                return(output)
        } else if (component_count == 1) {

            output <-
                output %>%
                rubix::filter_at_grepl(concept_name,
                                       grepl_phrase = " monotherapy") %>%
                rubix::arrange_by_nchar(nchar_col = concept_name)

            return(output)

        } else if (component_count == 2) {

                output %>%
                    rubix::filter_at_grepl(concept_name,
                                           grepl_phrase = " and ") %>%
                rubix::arrange_by_nchar(nchar_col = concept_name)
        } else if (component_count == 3) {


                output %>%
                        dplyr::mutate(comma_count = centipede::nchar_comma(concept_name)) %>%
                        dplyr::filter(comma_count == 2) %>%
                rubix::arrange_by_nchar(nchar_col = concept_name)

        } else if (component_count == 4) {

                output %>%
                    dplyr::mutate(comma_count = centipede::nchar_comma(concept_name)) %>%
                    dplyr::filter(comma_count == 3) %>%
                    rubix::arrange_by_nchar(nchar_col = concept_name)

        } else {
            max <-  1 + (output %>%
                        dplyr::transmute(comma_count = centipede::nchar_comma(concept_name)) %>%
                        unlist() %>%
                        max(na.rm = TRUE))
            warning('"component_count" max is: ', max,  ' returning unfiltered output')
            return(output)

        }
    }

