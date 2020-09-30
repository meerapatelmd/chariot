





calculate_drug_strength <-
        function(conn = NULL) {

                # test_data <- chariot::queryAthena("SELECT *
                #                                   FROM public.drug_strength
                #                                   ORDER BY RANDOM()
                #                                   LIMIT 10000")
                #
                #
                # conn <- chariot::connectAthena()

                units_concepts <-
                        queryAthena("SELECT
                                        concept_id,
                                        concept_name
                                    FROM public.concept
                                    WHERE domain_id = 'Unit'
                                        AND vocabulary_id = 'UCUM'
                                        AND concept_class_id = 'Unit'
                                        AND invalid_reason IS NULL;",
                                    conn = conn,
                                    cache_only = FALSE,
                                    skip_cache = TRUE,
                                    override_cache = FALSE,
                                    render_sql = FALSE,
                                    verbose = FALSE,
                                    sleepTime = 1)


                # drug_strength with hours denominators
                rate_concepts <-
                        queryAthena("SELECT DISTINCT c.*
                                    FROM public.drug_strength ds
                                    LEFT JOIN public.concept c
                                    ON c.concept_id = ds.drug_concept_id
                                    WHERE ds.denominator_unit_concept_id = 8505",
                                    conn = conn,
                                    cache_only = FALSE,
                                    skip_cache = TRUE,
                                    override_cache = FALSE,
                                    render_sql = FALSE,
                                    verbose = FALSE,
                                    sleepTime = 1)


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



                # Adding numerator and denominator  unit concept names
                #
                data2 <-
                dplyr::left_join(test_data,
                                test_data %>%
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
                                                            values_drop_na = TRUE) %>%
                                        dplyr::left_join(units_concepts) %>%
                                        tidyr::pivot_wider(id_cols = c(drug_concept_id, ingredient_concept_id),
                                                           names_from = field,
                                                           values_from = concept_name) %>%
                                        dplyr::rename_at(vars(c(amount_unit_concept_id,
                                                                numerator_unit_concept_id,
                                                                denominator_unit_concept_id)),
                                                         ~stringr::str_replace_all(., "[_]{1}id$", "_name"))) %>%
                        dplyr::distinct()

                # Combine unit names
                data3 <-
                        data2 %>%
                        tidyr::unite(col = "calculated_value_units",
                                     numerator_unit_concept_name,
                                     denominator_unit_concept_name,
                                     sep = "/",
                                     na.rm = TRUE)

                data4 <-
                        data3 %>%
                        dplyr::mutate(denominator_value = ifelse(!is.na(numerator_value) & is.na(denominator_value), 1, denominator_value)) %>%
                        dplyr::mutate(calculated_value = numerator_value/denominator_value)


                data4 %>%
                        dplyr::mutate(drug_strength = coalesce(amount_value, calculated_value),
                                      drug_strength_unit = coalesce(amount_unit_concept_name, calculated_value_units)) %>%
                        dplyr::select(drug_concept_id,
                                      ingredient_concept_id,
                                      drug_strength,
                                      drug_strength_unit,
                                      dplyr::everything())

        }
