#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param kind PARAM_DESCRIPTION, Default: c("LEFT", "RIGHT", "INNER", "FULL")
#' @param data PARAM_DESCRIPTION
#' @param column PARAM_DESCRIPTION, Default: NULL
#' @param select_data_columns PARAM_DESCRIPTION, Default: '*'
#' @param distinct PARAM_DESCRIPTION, Default: FALSE
#' @param write_schema PARAM_DESCRIPTION, Default: 'patelm9'
#' @param vocab_schema PARAM_DESCRIPTION, Default: 'omop_vocabulary'
#' @param where_in_concept_field PARAM_DESCRIPTION
#' @param where_in_concept_field_value PARAM_DESCRIPTION
#' @param where_not_in_concept_field PARAM_DESCRIPTION
#' @param where_not_in_concept_field_value PARAM_DESCRIPTION
#' @param where_is_null_concept_field PARAM_DESCRIPTION, Default: 'invalid_reason'
#' @param where_is_not_null_concept_field PARAM_DESCRIPTION
#' @param conn PARAM_DESCRIPTION
#' @param conn_fun PARAM_DESCRIPTION, Default: 'connectAthena()'
#' @param verbose PARAM_DESCRIPTION, Default: TRUE
#' @param render_sql PARAM_DESCRIPTION, Default: TRUE
#' @param render_only PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
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



#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @inheritParams join_on_concept_id
#' @rdname join_on_concept_code
#' @export
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


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @inheritParams join_on_concept_id
#' @rdname join_on_concept_name
#' @export
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



#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @inheritParams join_on_concept_id
#' @param where_in_concept_synonym_field PARAM_DESCRIPTION
#' @param where_in_concept_synonym_field_value PARAM_DESCRIPTION
#' @param where_not_in_concept_synonym_field PARAM_DESCRIPTION
#' @param where_not_in_concept_synonym_field_value PARAM_DESCRIPTION
#' @param where_is_null_concept_synonym_field PARAM_DESCRIPTION
#' @param where_is_not_null_concept_synonym_field PARAM_DESCRIPTION
#' @rdname join_on_concept_synonym_name
#' @export
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
                     vocab_table = "concept",
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

