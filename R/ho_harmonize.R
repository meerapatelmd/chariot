#' @title
#' Reduce All HemOnc Concepts to Component
#' @description
#' This function takes a mix of HemOnc Regimen and HemOnc Component Concepts and
#' returns back all the unique HemOnc Components associated with the combination.
#' @param ... HemOnc Concept Class objects or Concept Ids of
#' either Regimen or Component concept classes.
#' @seealso
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{select}}
#' @rdname ho_reduce_to_components
#' @export
#' @importFrom dplyr filter select


ho_reduce_to_components <-
        function(...,
                 check_validity = TRUE,
                 vocab_schema = "omop_vocabulary",
                 conn,
                 conn_fun = "connectAthena()",
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 render_sql = TRUE,
                 verbose = TRUE,
                 sleepTime = 1) {


                if (missing(conn)) {

                        conn <- eval(rlang::parse_expr(conn_fun))
                        on.exit(expr = dcAthena(conn = conn),
                                add = TRUE,
                                after = TRUE)
                }

                hemonc_concept_objs <- unlist(rlang::list2(...))
                hemonc_concept_ids <- vector()
                for (i in seq_along(hemonc_concept_objs)) {
                        hemonc_concept_obj <- hemonc_concept_objs[[i]]
                        if (class(hemonc_concept_obj) == "concept") {
                                hemonc_concept_ids <-
                                        c(hemonc_concept_ids,
                                          hemonc_concept_obj@concept_id)
                        } else {
                                hemonc_concept_ids <-
                                        c(hemonc_concept_ids,
                                          hemonc_concept_obj)
                        }
                }


                if (check_validity) {
                        if (verbose) {
                                cli::cli_rule(left = "Checking Validity")
                        }


                        sql_statement <-
                                SqlRender::render(
                                        "
                            SELECT *
                            FROM @vocab_schema.concept c
                            WHERE c.concept_id IN (@hemonc_concept_ids)
                                    AND c.invalid_reason IS NULL
                                    AND c.concept_class_id IN ('Regimen', 'Component')
                                    AND c.vocabulary_id = 'HemOnc'
                            ",
                                        vocab_schema = vocab_schema,
                                        hemonc_concept_ids = hemonc_concept_ids
                                )

                        output <- queryAthena(
                                sql_statement = sql_statement,
                                conn = conn,
                                cache_only = cache_only,
                                skip_cache = skip_cache,
                                override_cache = override_cache,
                                render_sql = render_sql,
                                verbose = verbose,
                                sleepTime = sleepTime
                        )

                        if (nrow(output) != length(hemonc_concept_ids)) {
                                invalid_ids <- hemonc_concept_ids[!(hemonc_concept_ids %in% output$concept_id)]
                                stop("Invalid concept ids: %s", paste(invalid_ids, collapse = ", "))
                        }
                }


                # If any of the concept_ids are regimens, to get their antineoplastic components
                input_concept <-
                        join_on_concept_id(
                                data = tibble::tibble(hemonc_concept_ids = hemonc_concept_ids)
                                )

                input_regimens <- input_concept %>%
                        dplyr::filter(concept_class_id == "Regimen")
                input_components <- input_concept %>%
                        dplyr::filter(concept_class_id == "Component")


                if (nrow(input_regimens) > 0) {
                        component_concept_ids_a <-
                                ho_lookup_antineoplastics(input_regimens$concept_id,
                                                          vocab_schema = vocab_schema,
                                                          check_validity = FALSE) %>%
                                dplyr::select(has_antineoplastic_concept_id) %>%
                                unlist()
                } else {
                        component_concept_ids_a <- vector()
                }


                if (nrow(input_components) > 0) {
                        component_concept_ids_b <- input_components$concept_id
                } else {
                        component_concept_ids_b <- vector()
                }

                component_ids <- c(component_concept_ids_a,
                               component_concept_ids_b)

                join_on_concept_id(data = tibble::tibble(component_id = component_ids)) %>%
                        dplyr::select(-component_id)
        }
