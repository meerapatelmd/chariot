#' #' @title
#' #' Get Test Data FF
#' #'
#' #' @description FUNCTION_DESCRIPTION
#' #' @param table PARAM_DESCRIPTION
#' #' @param ... PARAM_DESCRIPTION
#' #' @return OUTPUT_DESCRIPTION
#' #' @details DETAILS
#' #' @examples
#' #' \dontrun{
#' #' if(interactive()){
#' #'  #EXAMPLE1
#' #'  }
#' #' }
#' #' @seealso
#' #'  \code{\link[SqlRender]{render}}
#' #' @rdname get_test_data_ff
#' #' @export
#' #' @importFrom SqlRender render
#'
#'
#' get_test_data_ff <-
#'         function(table,
#'                  ...) {
#'
#'
#'
#'                 if (!missing(...)) {
#'
#'                         Args  <- list(...)
#'
#'                         nms <- names(Args)
#'                         vals <- unname(Args)
#'
#'                         output <- list()
#'
#'                         for (i in seq_along(vals)) {
#'
#'                                 output[[i]] <- list()
#'
#'                                 val <- vals[[i]]
#'
#'                                 if ("NULL" %in% val) {
#'
#'                                         output[[i]][[1]] <-
#'                                                 paste0(nms[i], " IS NULL")
#'
#'                                 }
#'
#'
#'                                 val <- val[!(val %in% c("NULL"))]
#'
#'
#'                                 if (length(val)>0) {
#'
#'                                         if (is.character(val)) {
#'
#'                                                 val <- paste0("'", val, "'")
#'
#'                                         }
#'
#'
#'                                         output[[i]][[2]] <-
#'                                                 SqlRender::render("@nm IN (@val)", nm = nms[i], val = val)
#'
#'
#'                                 }
#'
#'                                 output[[i]] <-
#'                                         unlist(output[[i]]) %>%
#'                                         paste(collapse = " OR ")
#'
#'
#'                                 output[[i]] <-
#'                                         paste0("(", output[[i]], ")")
#'
#'
#'                         }
#'
#'                         where_clause1 <-
#'                                 paste0(unlist(output) %>%
#'                                                paste(collapse = " AND "))
#'
#'                 } else {
#'
#'                         where_clause1 <- NULL
#'
#'
#'
#'                 }
#'
#'
#'                 function(...,
#'                          schema,
#'                          fields = NULL,
#'                          limit = 100,
#'                          cache_only = FALSE,
#'                          skip_cache = FALSE,
#'                          override_cache = TRUE,
#'                          verbose = TRUE,
#'                          render_sql = TRUE) {
#'
#'                         if (missing(schema)) {
#'
#'                                 stop("`schema` is required")
#'
#'                         }
#'
#'
#'                         if (!missing(...)) {
#'
#'                                 Args  <- list(...)
#'
#'                                 nms <- names(Args)
#'                                 vals <- unname(Args)
#'
#'                                 output <- list()
#'
#'                                 for (i in seq_along(vals)) {
#'
#'                                         output[[i]] <- list()
#'
#'                                         val <- vals[[i]]
#'
#'                                         if ("NULL" %in% val) {
#'
#'                                                 output[[i]][[1]] <-
#'                                                         paste0(nms[i], " IS NULL")
#'
#'                                         }
#'
#'
#'                                         val <- val[!(val %in% c("NULL"))]
#'
#'
#'                                         if (length(val)>0) {
#'
#'                                                 if (is.character(val)) {
#'
#'                                                         val <- paste0("'", val, "'")
#'
#'                                                 }
#'
#'
#'                                                 output[[i]][[2]] <-
#'                                                         SqlRender::render("@nm IN (@val)", nm = nms[i], val = val)
#'
#'
#'                                         }
#'
#'                                         output[[i]] <-
#'                                                 unlist(output[[i]]) %>%
#'                                                 paste(collapse = " OR ")
#'
#'
#'                                         output[[i]] <-
#'                                                 paste0("(", output[[i]], ")")
#'
#'
#'                                 }
#'
#'                                 where_clause2 <-
#'                                         paste0(unlist(output) %>%
#'                                                        paste(collapse = " AND "))
#'
#'                         } else {
#'
#'                                 where_clause2 <- NULL
#'
#'
#'
#'                         }
#'
#'                         where_clause <- paste0("WHERE ", c(where_clause1,
#'                                                            where_clause2) %>%
#'                                                        paste(collapse = " AND "))
#'
#'
#'
#'                         if (is.null(fields)) {
#'
#'                                 fields <-
#'                                         pg13::ls_fields(conn = conn,
#'                                                         table = table,
#'                                                         schema = schema,
#'                                                         verbose = verbose,
#'                                                         render_sql = render_sql)
#'
#'
#'                         }
#'
#'
#'                         select_statement <-
#'                                 paste0(fields, " AS ", "test_", fields) %>%
#'                                 paste(collapse = ",\n")
#'
#'
#'                         sql_statement <-
#'                                 SqlRender::render(
#'                                         "
#'                                         SELECT @select_statement
#'                                         FROM @schema.@table
#'                                         @where_clause
#'                                         ORDER BY RANDOM()
#'                                         LIMIT @limit
#'                                         ",
#'                                         select_statement = select_statement,
#'                                         schema = schema,
#'                                         table = table,
#'                                         where_clause = where_clause,
#'                                         limit = limit
#'                                 )
#'
#'
#'                         queryAthena(sql_statement = sql_statement,
#'                               cache_only = cache_only,
#'                               skip_cache = skip_cache,
#'                               override_cache = override_cache,
#'                               verbose = verbose,
#'                               render_sql = render_sql
#'                         )
#'
#'
#'                 }
#'
#'
#'         }
#'
#'
#' #' @title FUNCTION_TITLE
#' #' @description FUNCTION_DESCRIPTION
#' #' @param ... PARAM_DESCRIPTION
#' #' @param schema PARAM_DESCRIPTION
#' #' @param fields PARAM_DESCRIPTION, Default: NULL
#' #' @param limit PARAM_DESCRIPTION, Default: 100
#' #' @param cache_only PARAM_DESCRIPTION, Default: FALSE
#' #' @param skip_cache PARAM_DESCRIPTION, Default: FALSE
#' #' @param override_cache PARAM_DESCRIPTION, Default: TRUE
#' #' @param verbose PARAM_DESCRIPTION, Default: TRUE
#' #' @param render_sql PARAM_DESCRIPTION, Default: TRUE
#' #' @param hrs_expired PARAM_DESCRIPTION, Default: 16
#' #' @return OUTPUT_DESCRIPTION
#' #' @details DETAILS
#' #' @examples
#' #' \dontrun{
#' #' if(interactive()){
#' #'  #EXAMPLE1
#' #'  }
#' #' }
#' #' @seealso
#' #'  \code{\link[SqlRender]{render}}
#' #' @rdname get_test_concepts
#' #' @export
#' #' @importFrom SqlRender render
#'
#'
#' get_test_concepts <-
#'         get_test_data_ff(table = "concept",
#'                          invalid_reason = c("NULL"))
#'
#'
#' #' @title FUNCTION_TITLE
#' #' @description FUNCTION_DESCRIPTION
#' #' @param ... PARAM_DESCRIPTION
#' #' @param schema PARAM_DESCRIPTION
#' #' @param fields PARAM_DESCRIPTION, Default: NULL
#' #' @param limit PARAM_DESCRIPTION, Default: 100
#' #' @param cache_only PARAM_DESCRIPTION, Default: FALSE
#' #' @param skip_cache PARAM_DESCRIPTION, Default: FALSE
#' #' @param override_cache PARAM_DESCRIPTION, Default: TRUE
#' #' @param verbose PARAM_DESCRIPTION, Default: TRUE
#' #' @param render_sql PARAM_DESCRIPTION, Default: TRUE
#' #' @param hrs_expired PARAM_DESCRIPTION, Default: 16
#' #' @return OUTPUT_DESCRIPTION
#' #' @details DETAILS
#' #' @examples
#' #' \dontrun{
#' #' if(interactive()){
#' #'  #EXAMPLE1
#' #'  }
#' #' }
#' #' @seealso
#' #'  \code{\link[SqlRender]{render}}
#' #' @rdname test.drug_concept
#' #' @export
#' #' @importFrom SqlRender render
#'
#'
#'
#' get_test_drug_concepts <-
#'         get.test_data_ff(table = "concept",
#'                          domain_id = "Drug",
#'                          vocabulary_id = c("RxNorm", "RxNorm Extension"),
#'                          standard_concept = c("NULL", "S"),
#'                          invalid_reason = c("NULL"))
#'
#'
#'
#' #' @title FUNCTION_TITLE
#' #' @description FUNCTION_DESCRIPTION
#' #' @param ... PARAM_DESCRIPTION
#' #' @param schema PARAM_DESCRIPTION
#' #' @param fields PARAM_DESCRIPTION, Default: NULL
#' #' @param limit PARAM_DESCRIPTION, Default: 100
#' #' @param cache_only PARAM_DESCRIPTION, Default: FALSE
#' #' @param skip_cache PARAM_DESCRIPTION, Default: FALSE
#' #' @param override_cache PARAM_DESCRIPTION, Default: TRUE
#' #' @param verbose PARAM_DESCRIPTION, Default: TRUE
#' #' @param render_sql PARAM_DESCRIPTION, Default: TRUE
#' #' @param hrs_expired PARAM_DESCRIPTION, Default: 16
#' #' @return OUTPUT_DESCRIPTION
#' #' @details DETAILS
#' #' @examples
#' #' \dontrun{
#' #' if(interactive()){
#' #'  #EXAMPLE1
#' #'  }
#' #' }
#' #' @seealso
#' #'  \code{\link[SqlRender]{render}}
#' #' @rdname get_test_drug_classes
#' #' @export
#' #' @importFrom SqlRender render
#'
#'
#'
#'
#' get_test_drug_classes <-
#'         get.test_data_ff(table = "concept",
#'                          domain_id = "Drug",
#'                          vocabulary_id = c("ATC", "HemOnc"),
#'                          standard_concept = c("C"),
#'                          invalid_reason = c("NULL"))
#'
#' #' @title
#' #' Get CDM Table Concept Id Fields
#' #'
#' #' @return
#' #' Tibble of the concept id fields for each CDM Table
#' #'
#' #' @importFrom tibble tribble
#' #' @export
#' #' @rdname get_concept_id_fields
#' #' @family get functions
#'
#' get_concept_id_fields <-
#'         function(...) {
#'
#'                 x <-
#'                         list(
#'                                 ATTRIBUTE_DEFINITION = ("attribute_type_concept_id"),
#'                                 CARE_SITE = c("place_of_service_concept_id"),
#'                                 COHORT = c("drug_concept_id"),
#'                                 COHORT_ATTRIBUTE = c("value_as_concept_id"),
#'                                 COHORT_DEFINITION = c("definition_type_concept_id", "subject_concept_id"),
#'                                 CONDITION_ERA = c("condition_concept_id"),
#'                                 CONDITION_OCCURRENCE = c("condition_concept_id", "condition_type_concept_id", "condition_source_concept_id", "condition_status_concept_id"),
#'                                 COST = c("cost_type_concept_id", "currency_concept_id", "revenue_code_concept_id", "drg_concept_id"),
#'                                 DEATH = c("death_type_concept_id", "cause_concept_id", "cause_source_concept_id"),
#'                                 DEVICE_EXPOSURE = c("device_concept_id", "device_type_concept_id", "device_source_concept_id"),
#'                                 DOSE_ERA = c("drug_concept_id", "unit_concept_id"),
#'                                 DRUG_ERA = c("drug_concept_id"),
#'                                 DRUG_EXPOSURE = c("drug_concept_id", "drug_type_concept_id", "route_concept_id", "drug_source_concept_id"),
#'                                 EPISODE = c("episode_concept_id", "episode_object_concept_id", "episode_type_concept_id", "episode_source_concept_id"),
#'                                 EPISODE_EVENT = c("episode_event_field_concept_id"),
#'                                 FACT_RELATIONSHIP = c("domain_concept_id_1", "domain_concept_id_2", "relationship_concept_id"),
#'                                 MEASUREMENT = c("measurement_concept_id", "measurement_type_concept_id", "operator_concept_id", "value_as_concept_id", "unit_concept_id", "measurement_source_concept_id", "modifier_of_field_concept_id"),
#'                                 METADATA = c("metadata_concept_id", "metadata_type_concept_id", "value_as_concept_id"),
#'                                 NOTE = c("note_type_concept_id", "note_class_concept_id", "encoding_concept_id", "language_concept_id"),
#'                                 NOTE_NLP = c("section_concept_id", "note_nlp_concept_id", "note_nlp_source_concept_id"),
#'                                 OBSERVATION = c("observation_concept_id", "observation_type_concept_id", "value_as_concept_id", "qualifier_concept_id", "unit_concept_id", "observation_source_concept_id"),
#'                                 OBSERVATION_PERIOD = c("period_type_concept_id"),
#'                                 PAYER_PLAN_PERIOD = c("payer_concept_id", "payer_source_concept_id", "plan_concept_id", "plan_source_concept_id", "sponsor_concept_id", "sponsor_source_concept_id", "stop_reason_concept_id", "stop_reason_source_concept_id"),
#'                                 PERSON = c("gender_concept_id", "race_concept_id", "ethnicity_concept_id", "gender_source_concept_id", "race_source_concept_id", "ethnicity_source_concept_id"),
#'                                 PROCEDURE_OCCURRENCE = c("procedure_concept_id", "procedure_type_concept_id", "modifier_concept_id", "procedure_source_concept_id"),
#'                                 PROVIDER = c("specialty_concept_id", "gender_concept_id", "specialty_source_concept_id", "gender_source_concept_id"),
#'                                 REGIMEN = c("drug_concept_id"),
#'                                 SOURCE_TO_CONCEPT_MAP = c("source_concept_id", "target_concept_id"),
#'                                 SPECIMEN = c("specimen_concept_id", "specimen_type_concept_id", "unit_concept_id", "anatomic_site_concept_id", "disease_status_concept_id"),
#'                                 VISIT_DETAIL = c("visit_detail_concept_id", "visit_detail_type_concept_id", "admitting_source_concept_id", "discharge_to_concept_id", "visit_detail_source_concept_id"),
#'                                 VISIT_OCCURRENCE = c("visit_concept_id", "visit_type_concept_id", "visit_source_concept_id", "admitting_source_concept_id", "discharge_to_concept_id")
#'                         )
#'
#'
#'                 if (missing(...)) {
#'
#'                         x
#'
#'                 } else {
#'
#'                         x[names(x) %in% unlist(rlang::list2(...))]
#'                 }
#'
#'
#'         }
