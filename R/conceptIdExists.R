#' QA to make sure the concept_id exists in OMOP
#' @export


conceptIdExists <-
    function(concept_id,
             schema) {

                    x <- queryConceptId(concept_ids = concept_id,
                                        schema = schema,
                                        override_cache = TRUE)

                    if (nrow(x) > 0) {

                            TRUE

                    } else {

                           FALSE
                    }

    }
