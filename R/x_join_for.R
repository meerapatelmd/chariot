#' @title
#' Join For Concept Synonyms
#'
#' @description
#' `join_for_*` functions differ from `join_on_*` functions in that `join_for_*`
#' joins on a vocabulary table field that is already specified, and that the join
#' is to add a specific field to the data, this case being the
#' `concept_synonym_name` field with a join on the `concept_id` field.
#'
#' @rdname join_for_concept_synonym_name
#' @export
#' @example inst/example/join.R

join_for_concept_synonym_name <-
        function(kind = c("LEFT", "RIGHT", "INNER", "FULL"),
                 data,
                 concept_id_column = NULL,
                 select_data_columns = "*",
                 select_concept_synonym_fields = c("concept_id",
                                                   "concept_synonym_name"),
                 distinct = FALSE,
                 write_schema = "patelm9",
                 vocab_schema = "omop_vocabulary",
                 where_in_concept_synonym_field = "language_concept_id",
                 where_in_concept_synonym_field_value = 4180186,
                 where_not_in_concept_synonym_field,
                 where_not_in_concept_synonym_field_value,
                 where_is_null_concept_synonym_field,
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
                     column = concept_id_column,
                     vocab_table = "concept_synonym",
                     vocab_field = "concept_id",
                     select_data_columns = select_data_columns,
                     select_vocab_fields = select_concept_synonym_fields,
                     distinct = distinct,
                     write_schema = write_schema,
                     vocab_schema = vocab_schema,
                     where_in_vocab_field = where_in_concept_synonym_field,
                     where_in_vocab_field_value =
                             where_in_concept_synonym_field_value,
                     where_not_in_vocab_field =
                             where_not_in_concept_synonym_field,
                     where_not_in_vocab_field_value =
                             where_not_in_concept_synonym_field_value,
                     where_is_null_vocab_field =
                             where_is_null_concept_synonym_field,
                     where_is_not_null_vocab_field =
                             where_is_not_null_concept_synonym_field,
                     case_insensitive = case_insensitive,
                     conn = conn,
                     conn_fun = conn_fun,
                     verbose = verbose,
                     render_sql = render_sql,
                     render_only = render_only)
        }

#' @title
#' Join For Descendant Ids
#'
#' @description
#' `join_for_*` functions differ from `join_on_*` functions in that `join_for_*`
#' joins on a vocabulary table field that is already specified, and that the join
#' is to add a specific field to the data, this case being the
#' `descendant_concept_id` field with a join on the `ancestor_concept_id` field.
#'
#' @rdname join_for_descendant_ids
#' @export
#' @example inst/example/join.R


join_for_descendant_ids <-
        function(kind = c("LEFT", "RIGHT", "INNER", "FULL"),
                 data,
                 ancestor_id_column = NULL,
                 select_data_columns = "*",
                 distinct = FALSE,
                 write_schema = "patelm9",
                 vocab_schema = "omop_vocabulary",
                 where_in_concept_ancestor_field,
                 where_in_concept_ancestor_field_value,
                 where_not_in_concept_ancestor_field,
                 where_not_in_concept_ancestor_field_value,
                 where_is_null_concept_ancestor_field,
                 where_is_not_null_concept_ancestor_field,
                 case_insensitive = TRUE,
                 conn,
                 conn_fun = "connectAthena()",
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE
        ) {


                join(kind = kind,
                     data = data,
                     column = ancestor_id_column,
                     vocab_table = "concept_ancestor",
                     vocab_field = "ancestor_concept_id",
                     select_data_columns = select_data_columns,
                     select_vocab_fields = c("descendant_concept_id",
                                             "min_levels_of_separation",
                                             "max_levels_of_separation"),
                     distinct = distinct,
                     write_schema = write_schema,
                     vocab_schema = vocab_schema,
                     where_in_vocab_field =
                             where_in_concept_ancestor_field,
                     where_in_vocab_field_value =
                             where_in_concept_ancestor_field_value,
                     where_not_in_vocab_field =
                             where_not_in_concept_ancestor_field,
                     where_not_in_vocab_field_value =
                             where_not_in_concept_ancestor_field_value,
                     where_is_null_vocab_field =
                             where_is_null_concept_ancestor_field,
                     where_is_not_null_vocab_field =
                             where_is_not_null_concept_ancestor_field,
                     case_insensitive = case_insensitive,
                     conn = conn,
                     conn_fun = conn_fun,
                     verbose = verbose,
                     render_sql = render_sql,
                     render_only = render_only)
        }

#' @title
#' Join For Ancestors
#'
#' @description
#' Perform a compound join of \code{\link{join_for_ancestor_ids}} and
#' \code{\link{join_on_concept_id}} for the ancestor concept ids to get all
#' the ancestor attributes along with the ancestor ids.
#'
#' @rdname join_for_ancestors
#' @export
#' @example inst/example/join.R
#' @importFrom dplyr select distinct rename_all left_join

join_for_descendants <-
        function(kind = c("LEFT", "RIGHT", "INNER", "FULL"),
                 data,
                 ancestor_id_column = NULL,
                 select_data_columns = "*",
                 distinct = FALSE,
                 write_schema = "patelm9",
                 vocab_schema = "omop_vocabulary",
                 where_in_concept_ancestor_field,
                 where_in_concept_ancestor_field_value,
                 where_not_in_concept_ancestor_field,
                 where_not_in_concept_ancestor_field_value,
                 where_is_null_concept_ancestor_field,
                 where_is_not_null_concept_ancestor_field,
                 case_insensitive = TRUE,
                 conn,
                 conn_fun = "connectAthena()",
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE) {

                descendant_ids <-
                        join_for_descendant_ids(
                                kind = kind,
                                data = data,
                                ancestor_id_column = ancestor_id_column,
                                select_data_columns = select_data_columns,
                                distinct = distinct,
                                write_schema = write_schema,
                                vocab_schema = vocab_schema,
                                where_in_concept_ancestor_field = where_in_concept_ancestor_field,
                                where_in_concept_ancestor_field_value = where_in_concept_ancestor_field_value,
                                where_not_in_concept_ancestor_field = where_not_in_concept_ancestor_field,
                                where_not_in_concept_ancestor_field_value = where_not_in_concept_ancestor_field_value,
                                where_is_null_concept_ancestor_field = where_is_null_concept_ancestor_field,
                                where_is_not_null_concept_ancestor_field = where_is_not_null_concept_ancestor_field,
                                case_insensitive = case_insensitive,
                                conn = conn,
                                conn_fun = conn_fun,
                                verbose = verbose,
                                render_sql = render_sql,
                                render_only = render_only
                        )

                descendant_concepts <-
                        join_on_concept_id(kind = kind,
                                           data = descendant_ids %>%
                                                   dplyr::select(descendant_concept_id),
                                           select_data_columns = select_data_columns,
                                           distinct = distinct,
                                           write_schema = write_schema,
                                           vocab_schema = vocab_schema,
                                           conn = conn,
                                           conn_fun = conn_fun,
                                           verbose = verbose,
                                           render_sql = render_sql,
                                           render_only = render_only) %>%
                        dplyr::select(-descendant_concept_id) %>%
                        dplyr::distinct() %>%
                        dplyr::rename_all(~ paste0("descendant_", .))

                descendant_ids %>%
                        dplyr::left_join(descendant_concepts,
                                         by = "descendant_concept_id",
                                         keep = TRUE) %>%
                        dplyr::select(-descendant_concept_id.x) %>%
                        dplyr::rename(descendant_concept_id = descendant_concept_id.y)
        }


#' @title
#' Join For Ancestor Ids
#'
#' @description
#' `join_for_*` functions differ from `join_on_*` functions in that `join_for_*`
#' joins on a vocabulary table field that is already specified, and that the join
#' is to add a specific field to the data, this case being the
#' `ancestor_concept_id` field with a join on the `descendant_concept_id` field.
#'
#' @rdname join_for_ancestor_ids
#' @export
#' @example inst/example/join.R


join_for_ancestor_ids <-
        function(kind = c("LEFT", "RIGHT", "INNER", "FULL"),
                 data,
                 descendant_id_column = NULL,
                 select_data_columns = "*",
                 distinct = FALSE,
                 write_schema = "patelm9",
                 vocab_schema = "omop_vocabulary",
                 where_in_concept_ancestor_field,
                 where_in_concept_ancestor_field_value,
                 where_not_in_concept_ancestor_field,
                 where_not_in_concept_ancestor_field_value,
                 where_is_null_concept_ancestor_field,
                 where_is_not_null_concept_ancestor_field,
                 case_insensitive = TRUE,
                 conn,
                 conn_fun = "connectAthena()",
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE
        ) {


                join(kind = kind,
                     data = data,
                     column = descendant_id_column,
                     vocab_table = "concept_ancestor",
                     vocab_field = "descendant_concept_id",
                     select_data_columns = select_data_columns,
                     select_vocab_fields = c("ancestor_concept_id",
                                             "min_levels_of_separation",
                                             "max_levels_of_separation"),
                     distinct = distinct,
                     write_schema = write_schema,
                     vocab_schema = vocab_schema,
                     where_in_vocab_field =
                             where_in_concept_ancestor_field,
                     where_in_vocab_field_value =
                             where_in_concept_ancestor_field_value,
                     where_not_in_vocab_field =
                             where_not_in_concept_ancestor_field,
                     where_not_in_vocab_field_value =
                             where_not_in_concept_ancestor_field_value,
                     where_is_null_vocab_field =
                             where_is_null_concept_ancestor_field,
                     where_is_not_null_vocab_field =
                             where_is_not_null_concept_ancestor_field,
                     case_insensitive = case_insensitive,
                     conn = conn,
                     conn_fun = conn_fun,
                     verbose = verbose,
                     render_sql = render_sql,
                     render_only = render_only)
        }


#' @title
#' Join For Ancestors
#'
#' @description
#' Perform a compound join of \code{\link{join_for_ancestor_ids}} and
#' \code{\link{join_on_concept_id}} for the ancestor concept ids to get all
#' the ancestor attributes along with the ancestor ids.
#'
#' @rdname join_for_ancestors
#' @export
#' @example inst/example/join.R
#' @importFrom dplyr select distinct rename_all left_join

join_for_ancestors <-
        function(kind = c("LEFT", "RIGHT", "INNER", "FULL"),
                 data,
                 descendant_id_column = NULL,
                 select_data_columns = "*",
                 distinct = FALSE,
                 write_schema = "patelm9",
                 vocab_schema = "omop_vocabulary",
                 where_in_concept_ancestor_field,
                 where_in_concept_ancestor_field_value,
                 where_not_in_concept_ancestor_field,
                 where_not_in_concept_ancestor_field_value,
                 where_is_null_concept_ancestor_field,
                 where_is_not_null_concept_ancestor_field,
                 case_insensitive = TRUE,
                 conn,
                 conn_fun = "connectAthena()",
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE) {

                ancestor_ids <-
                join_for_ancestor_ids(
                        kind = kind,
                        data = data,
                        descendant_id_column = descendant_id_column,
                        select_data_columns = select_data_columns,
                        distinct = distinct,
                        write_schema = write_schema,
                        vocab_schema = vocab_schema,
                        where_in_concept_ancestor_field = where_in_concept_ancestor_field,
                        where_in_concept_ancestor_field_value = where_in_concept_ancestor_field_value,
                        where_not_in_concept_ancestor_field = where_not_in_concept_ancestor_field,
                        where_not_in_concept_ancestor_field_value = where_not_in_concept_ancestor_field_value,
                        where_is_null_concept_ancestor_field = where_is_null_concept_ancestor_field,
                        where_is_not_null_concept_ancestor_field = where_is_not_null_concept_ancestor_field,
                        case_insensitive = case_insensitive,
                        conn = conn,
                        conn_fun = conn_fun,
                        verbose = verbose,
                        render_sql = render_sql,
                        render_only = render_only
                )

                ancestor_concepts <-
                join_on_concept_id(kind = kind,
                                   data = ancestor_ids %>%
                                           dplyr::select(ancestor_concept_id),
                                   select_data_columns = select_data_columns,
                                   distinct = distinct,
                                   write_schema = write_schema,
                                   vocab_schema = vocab_schema,
                                   conn = conn,
                                   conn_fun = conn_fun,
                                   verbose = verbose,
                                   render_sql = render_sql,
                                   render_only = render_only) %>%
                        dplyr::select(-ancestor_concept_id) %>%
                        dplyr::distinct() %>%
                        dplyr::rename_all(~ paste0("ancestor_", .))

                ancestor_ids %>%
                        dplyr::left_join(ancestor_concepts,
                                         by = "ancestor_concept_id",
                                         keep = TRUE) %>%
                        dplyr::select(-ancestor_concept_id.x) %>%
                        dplyr::rename(ancestor_concept_id = ancestor_concept_id.y)
        }
