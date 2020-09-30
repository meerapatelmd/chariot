#' @details Get all the units representation in the drug strength table to assess for any "rate" numerator/denominator unit comibinations such as x per hour.
#' @noRd

get_drug_strength_units <-
        function() {

                drug_strength_units_concepts <-
                        queryAthena("SELECT DISTINCT
                                                drug_concept_id,
                                                ingredient_concept_id,
                                                amount_unit_concept_id,
                                                numerator_unit_concept_id,
                                                denominator_unit_concept_id
                                                    FROM public.drug_strength;",
                                    conn = conn,
                                    cache_only = FALSE,
                                    skip_cache = TRUE,
                                    override_cache = FALSE,
                                    render_sql = FALSE,
                                    verbose = FALSE,
                                    sleepTime = 1)

                drug_strength_units_concepts <-
                        drug_strength_units_concepts %>%
                        dplyr::select(drug_concept_id,
                                      ingredient_concept_id,
                                      amount_unit_concept_id,
                                      numerator_unit_concept_id,
                                      denominator_unit_concept_id) %>%
                        dplyr::distinct() %>%
                        tidyr::pivot_longer(cols = c(amount_unit_concept_id,
                                                     numerator_unit_concept_id,
                                                     denominator_unit_concept_id),
                                            values_to = "concept_id",
                                            names_to = "field",
                                            values_drop_na = TRUE)  %>%
                        dplyr::rename(unit_concept_id = concept_id) %>%
                        chariot::leftJoinConcept(column = "unit_concept_id") %>%
                        dplyr::select(concept_id:last_col()) %>%
                        dplyr::distinct() %>%
                        dplyr::select(concept_id, concept_name)

        }

