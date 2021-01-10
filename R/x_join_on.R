#' @title
#' Join On Concept Id
#' @example inst/example/join.R
#' @rdname join_on_concept_id
#' @export
join_on_concept_id <-
        function(kind = c("LEFT", "RIGHT", "INNER", "FULL"),
                 data,
                 column = NULL,
                 select_data_columns = "*",
                 distinct = FALSE,
                 write_schema = "patelm9",
                 vocab_schema = "omop_vocabulary",
                 where_in_concept_field,
                 where_in_concept_field_value,
                 where_not_in_concept_field,
                 where_not_in_concept_field_value,
                 where_is_null_concept_field = "invalid_reason",
                 where_is_not_null_concept_field,
                 conn,
                 conn_fun = "connectAthena()",
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE
        ) {


                join(kind = kind,
                     data = data,
                     column = column,
                     vocab_table = "concept",
                     vocab_field = "concept_id",
                     select_data_columns = select_data_columns,
                     select_vocab_fields = "*",
                     distinct = distinct,
                     write_schema = write_schema,
                     vocab_schema = vocab_schema,
                     where_in_vocab_field = where_in_concept_field,
                     where_in_vocab_field_value = where_in_concept_field_value,
                     where_not_in_vocab_field = where_not_in_concept_field,
                     where_not_in_vocab_field_value = where_not_in_concept_field_value,
                     where_is_null_vocab_field = where_is_null_concept_field,
                     where_is_not_null_vocab_field = where_is_not_null_concept_field,
                     conn = conn,
                     conn_fun = conn_fun,
                     verbose = verbose,
                     render_sql = render_sql,
                     render_only = render_only)
        }



#' @title
#' Join On Concept Code
#' @inheritParams join_on_concept_id
#' @rdname join_on_concept_code
#' @export
#' @example inst/example/join.R
join_on_concept_code <-
        function(kind = c("LEFT", "RIGHT", "INNER", "FULL"),
                 data,
                 column = NULL,
                 select_data_columns = "*",
                 distinct = FALSE,
                 write_schema = "patelm9",
                 vocab_schema = "omop_vocabulary",
                 where_in_concept_field,
                 where_in_concept_field_value,
                 where_not_in_concept_field,
                 where_not_in_concept_field_value,
                 where_is_null_concept_field = "invalid_reason",
                 where_is_not_null_concept_field,
                 case_insensitive = TRUE,
                 conn,
                 conn_fun = "connectAthena()",
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE
        ) {


                join(kind = kind,
                     data = data,
                     column = column,
                     vocab_table = "concept",
                     vocab_field = "concept_code",
                     select_data_columns = select_data_columns,
                     select_vocab_fields = "*",
                     distinct = distinct,
                     write_schema = write_schema,
                     vocab_schema = vocab_schema,
                     where_in_vocab_field = where_in_concept_field,
                     where_in_vocab_field_value = where_in_concept_field_value,
                     where_not_in_vocab_field = where_not_in_concept_field,
                     where_not_in_vocab_field_value = where_not_in_concept_field_value,
                     where_is_null_vocab_field = where_is_null_concept_field,
                     where_is_not_null_vocab_field = where_is_not_null_concept_field,
                     case_insensitive = case_insensitive,
                     conn = conn,
                     conn_fun = conn_fun,
                     verbose = verbose,
                     render_sql = render_sql,
                     render_only = render_only)
        }


#' @title
#' Join On Concept Name
#' @inheritParams join_on_concept_id
#' @rdname join_on_concept_name
#' @export
#' @example inst/example/join.R
join_on_concept_name <-
        function(kind = c("LEFT", "RIGHT", "INNER", "FULL"),
                 data,
                 column = NULL,
                 select_data_columns = "*",
                 distinct = FALSE,
                 write_schema = "patelm9",
                 vocab_schema = "omop_vocabulary",
                 where_in_concept_field,
                 where_in_concept_field_value,
                 where_not_in_concept_field,
                 where_not_in_concept_field_value,
                 where_is_null_concept_field = "invalid_reason",
                 where_is_not_null_concept_field,
                 case_insensitive = TRUE,
                 conn,
                 conn_fun = "connectAthena()",
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE
        ) {


                join(kind = kind,
                     data = data,
                     column = column,
                     vocab_table = "concept",
                     vocab_field = "concept_name",
                     select_data_columns = select_data_columns,
                     select_vocab_fields = "*",
                     distinct = distinct,
                     write_schema = write_schema,
                     vocab_schema = vocab_schema,
                     where_in_vocab_field = where_in_concept_field,
                     where_in_vocab_field_value = where_in_concept_field_value,
                     where_not_in_vocab_field = where_not_in_concept_field,
                     where_not_in_vocab_field_value = where_not_in_concept_field_value,
                     where_is_null_vocab_field = where_is_null_concept_field,
                     where_is_not_null_vocab_field = where_is_not_null_concept_field,
                     case_insensitive = case_insensitive,
                     conn = conn,
                     conn_fun = conn_fun,
                     verbose = verbose,
                     render_sql = render_sql,
                     render_only = render_only)
        }



#' @title
#' Join On Concept Synonym Name
#' @inheritParams join_on_concept_id
#' @param where_in_concept_synonym_field PARAM_DESCRIPTION
#' @param where_in_concept_synonym_field_value PARAM_DESCRIPTION
#' @param where_not_in_concept_synonym_field PARAM_DESCRIPTION
#' @param where_not_in_concept_synonym_field_value PARAM_DESCRIPTION
#' @param where_is_null_concept_synonym_field PARAM_DESCRIPTION
#' @param where_is_not_null_concept_synonym_field PARAM_DESCRIPTION
#' @rdname join_on_concept_synonym_name
#' @export
#' @example inst/example/join.R
join_on_concept_synonym_name <-
        function(kind = c("LEFT", "RIGHT", "INNER", "FULL"),
                 data,
                 column = NULL,
                 select_data_columns = "*",
                 distinct = FALSE,
                 write_schema = "patelm9",
                 vocab_schema = "omop_vocabulary",
                 where_in_concept_synonym_field,
                 where_in_concept_synonym_field_value,
                 where_not_in_concept_synonym_field,
                 where_not_in_concept_synonym_field_value,
                 where_is_null_concept_synonym_field ,
                 where_is_not_null_concept_synonym_field,
                 case_insensitive = TRUE,
                 conn,
                 conn_fun = "connectAthena()",
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE
        ) {


                join(kind = kind,
                     data = data,
                     column = column,
                     vocab_table = "concept_synonym",
                     vocab_field = "concept_synonym_name",
                     select_data_columns = select_data_columns,
                     select_vocab_fields = "*",
                     distinct = distinct,
                     write_schema = write_schema,
                     vocab_schema = vocab_schema,
                     where_in_vocab_field = where_in_concept_synonym_field,
                     where_in_vocab_field_value = where_in_concept_synonym_field_value,
                     where_not_in_vocab_field = where_not_in_concept_synonym_field,
                     where_not_in_vocab_field_value = where_not_in_concept_synonym_field_value,
                     where_is_null_vocab_field = where_is_null_concept_synonym_field,
                     where_is_not_null_vocab_field = where_is_not_null_concept_synonym_field,
                     case_insensitive = case_insensitive,
                     conn = conn,
                     conn_fun = conn_fun,
                     verbose = verbose,
                     render_sql = render_sql,
                     render_only = render_only)
        }

