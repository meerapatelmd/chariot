#' @title
#' Lookup the Units for Drug Strength Processing
#' @description
#' To normalize the drug exposure calculations in the Drug Strength tables,
#' all units related to rate, commonly seen in extended release formulations,
#' or time, are excluded from the final calculation.

#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname ds_lookup_ucum
#' @export
#' @importFrom SqlRender render


ds_lookup_ucum <-
        function(conn,
                 conn_fun = "connectAthena()",
                 vocab_schema = "omop_vocabulary") {

                if (missing(conn)) {

                        conn <- eval(rlang::parse_expr(conn_fun))
                        on.exit(expr = dcAthena(conn = conn),
                                add = TRUE,
                                after = TRUE)
                }

        queryAthena(
                SqlRender::render(
                        "
                        SELECT *
                        FROM @vocab_schema.concept
                        WHERE
                                domain_id = 'Unit'
                                        AND vocabulary_id = 'UCUM'
                                        AND concept_class_id = 'Unit'
                                        AND invalid_reason IS NULL
                                        AND concept_id NOT IN (
                                                SELECT concept_id
                                                FROM @vocab_schema.concept
                                                WHERE domain_id = 'Unit'
                                                        AND vocabulary_id = 'UCUM'
                                                        AND concept_class_id = 'Unit'
                                                        AND invalid_reason IS NULL
                                                        AND (LOWER(concept_name) LIKE '%hour%'
                                                                OR LOWER(concept_name) LIKE '%minute%'
                                                                OR LOWER(concept_name) LIKE '%second%'
                                                                OR LOWER(concept_name) LIKE '%day%'
                                                                OR LOWER(concept_name) LIKE '%week%'
                                                                OR LOWER(concept_name) LIKE '%month%'
                                                                OR LOWER(concept_name) LIKE '%year%')
                                                );
                        ",
                vocab_schema = vocab_schema),
                conn = conn,
                cache_only = FALSE,
                skip_cache = TRUE,
                override_cache = FALSE,
                render_sql = FALSE,
                verbose = FALSE,
                sleepTime = 1
        )

        }
