#' @title
#' Does the Drug Strength Staged table exist?
#'
#' @description
#' Checks for the existence of the Drug Strength Staged table, which is a
#' prerequisite for any `ds_join_*` functions. If it does not exist, it can be
#' written using \code{\link{ds_process}} followed by \code{\link{ds_stage}}.
#'
#' @param ds_schema Schema for the Drug Strength Staged table, Default: 'patelm9'
#' @seealso
#'  \code{\link[rlang]{parse_expr}}
#'  \code{\link[pg13]{ls_tables}}
#' @rdname ds_staged_table_exists
#' @export
#' @importFrom rlang parse_expr
#' @importFrom pg13 ls_tables
ds_staged_table_exists <-
        function(conn,
                 conn_fun = "connectAthena()",
                 ds_schema = "patelm9") {

                if (missing(conn)) {

                        conn <- eval(rlang::parse_expr(conn_fun))
                        on.exit(expr = dcAthena(conn = conn),
                                add = TRUE,
                                after = TRUE)
                }

                tables <- pg13::ls_tables(conn = conn,
                                          schema = ds_schema)
                tables <- toupper(tables)

                "DRUG_STRENGTH_STAGED" %in% tables

        }




#' @title
#' Get the Staged Drug Strengths by Drug
#' @description
#' Join the staged calculations in the Drug Strength Staged table by
#' `drug_concept_id`.
#' @inheritParams ds_staged_table_exists
#' @rdname ds_join_on_drug
#' @export
ds_join_on_drug <-
        function(data,
                 drug_concept_id_col = "drug_concept_id",
                 conn,
                 conn_fun = "connectAthena()",
                 ds_schema = "patelm9",
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE) {


                if (!(ds_staged_table_exists(conn = conn,
                                       ds_schema = ds_schema))) {

                        stop(sprintf("'DRUG_STRENGTH_STAGED' table does not
                                     exist in '%s' schema. Run if `ds_process()`
                                     and `ds_stage()` if it hasn't been written.",
                                     ds_schema))
                }


                join(data = data,
                     column = drug_concept_id_col,
                     vocab_table = "drug_strength_staged",
                     vocab_field = "drug_concept_id")


        }




#' @title
#' Get the Staged Drug Strengths by Ingredient
#' @description
#' Join the staged calculations in the Drug Strength Staged table by
#' `ingredient_concept_id`.
#' @inheritParams ds_staged_table_exists
#' @rdname ds_join_on_ingredient
#' @export
ds_join_on_ingredient <-
        function(data,
                 ingredient_concept_id_col = "drug_concept_id",
                 conn,
                 conn_fun = "connectAthena()",
                 ds_schema = "patelm9",
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE) {


                if (!(ds_staged_table_exists(conn = conn,
                                             ds_schema = ds_schema))) {

                        stop(sprintf("'DRUG_STRENGTH_STAGED' table does not
                                     exist in '%s' schema. Run `ds_process()`
                                     and `ds_stage()` if it hasn't been written.",
                                     ds_schema))
                }


                join(data = data,
                     column = ingredient_concept_id_col,
                     vocab_table = "drug_strength_staged",
                     vocab_field = "ingredient_concept_id")


        }
