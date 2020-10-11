#' @title
#' List UCUM Unit Concepts
#' @description
#' List all UCUM Unit Concepts from the Concept Table. To list UCUM Units related to the measurement of Time, see \code{\link{list_time_unit_concepts}}. The resultset is never cached.
#' @param conn PARAM_DESCRIPTION, Default: NULL
#' @param vocabSchema PARAM_DESCRIPTION, Default: 'omop_vocabulary'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname list_time_unit_concepts
#' @export
#' @importFrom SqlRender render


list_unit_concepts <-
        function(conn = NULL,
                 vocabSchema = "omop_vocabulary") {

                        queryAthena(SqlRender::render(
                                "
                                        SELECT
                                                concept_id,
                                                concept_name
                                        FROM @vocabSchema.concept
                                        WHERE domain_id = 'Unit'
                                                AND vocabulary_id = 'UCUM'
                                                AND concept_class_id = 'Unit'
                                                AND invalid_reason IS NULL
                                        ;
                                        ",vocabSchema = vocabSchema),
                                conn = conn,
                                cache_only = FALSE,
                                skip_cache = TRUE,
                                override_cache = FALSE,
                                render_sql = FALSE,
                                verbose = FALSE,
                                sleepTime = 1)
        }

#' @title
#' List UCUM Concepts Related to Time
#' @description
#' List all UCUM Unit Concepts from the Concept Table that are related to measurement of Time based on a broad regex match to the phrases "hour", "minute", "second", "day", "month", "week", and "year". To list all UCUM Units, see \code{\link{list_unit_concepts}}. This resultset is never cached.
#' @param conn PARAM_DESCRIPTION, Default: NULL
#' @param vocabSchema PARAM_DESCRIPTION, Default: 'omop_vocabulary'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname list_time_unit_concepts
#' @export
#' @importFrom SqlRender render


list_time_unit_concepts <-
        function(conn = NULL,
                 vocabSchema = "omop_vocabulary") {

                queryAthena(SqlRender::render(
                        "
                                        SELECT
                                                concept_id,
                                                concept_name
                                        FROM @vocabSchema.concept
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
                                        ;
                                        ",vocabSchema = vocabSchema),
                        conn = conn,
                        cache_only = FALSE,
                        skip_cache = TRUE,
                        override_cache = FALSE,
                        render_sql = FALSE,
                        verbose = FALSE,
                        sleepTime = 1)
        }

#' @title
#' List OMOP Unit Concepts in the Drug Strength Table
#' @description
#' Unpivot all of the Drug Strength table unit concept id fields (amount_unit_concept_id, numerator_unit_concept_id, denominator_unit_concept_id) to a unit_concept_id_type field and return the Concept Table entry for each unit concept id.
#' @param conn PARAM_DESCRIPTION, Default: NULL
#' @param vocabSchema PARAM_DESCRIPTION, Default: 'omop_vocabulary'
#' @return OUTPUT_DESCRIPTION
#' @details
#' The resultset is not cached
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname list_drug_strength_units
#' @export
#' @importFrom SqlRender render

list_drug_strength_units <-
        function(conn = NULL,
                 vocabSchema = "omop_vocabulary") {

                queryAthena(SqlRender::render(
                        " WITH all_unit_concept_ids AS (
                                        SELECT
                                                unnest(array['amount_unit_concept_id', 'numerator_unit_concept_id', 'denominator_unit_concept_id']) AS unit_concept_id_type,
                                               unnest(array[amount_unit_concept_id, numerator_unit_concept_id, denominator_unit_concept_id]) AS unit_concept_id
                                        FROM @vocabSchema.drug_strength
                        ),
                        all_unit_concept_ids2 AS (
                                        SELECT DISTINCT *
                                        FROM all_unit_concept_ids
                                        WHERE unit_concept_id IS NOT NULL
                                        ORDER BY unit_concept_id_type
                        )

                        SELECT
                                u.unit_concept_id_type,
                                c.concept_id AS unit_concept_id,
                                c.concept_name AS unit_concept_name,
                                c.domain_id AS unit_domain_id,
                                c.vocabulary_id AS unit_vocabulary_id,
                                c.concept_class_id AS unit_concept_class_id,
                                c.standard_concept AS unit_standard_concept,
                                c.concept_code AS unit_concept_code,
                                c.valid_start_date AS unit_valid_start_date,
                                c.valid_end_date AS unit_valid_end_date,
                                c.invalid_reason AS unit_invalid_reason
                        FROM all_unit_concept_ids2 u
                        LEFT JOIN @vocabSchema.concept c
                        ON c.concept_id = u.unit_concept_id
                                        ;
                                        ",vocabSchema = vocabSchema),
                        conn = conn,
                        cache_only = FALSE,
                        skip_cache = TRUE,
                        override_cache = FALSE,
                        render_sql = FALSE,
                        verbose = FALSE,
                        sleepTime = 1)
        }




stage_drug_strength_formulas <-
        function(.data,
                 drug_concept_id_column,
                 include_rates = FALSE,
                 conn = NULL,
                 vocabSchema = "omop_vocabulary",
                 writeSchema) {

                # test_data <- chariot::queryAthena("SELECT *
                #                                   FROM public.concept
                #                                   WHERE domain_id = 'Drug' AND invalid_reason IS NULL
                #                                   ORDER BY RANDOM()
                #                                   LIMIT 10000")
                # vocabSchema = "omop_vocabulary"
                # conn <- chariot::connectAthena()

                # Write table to writeSchema for join
                if (is.null(conn)) {
                        write_conn <- connectAthena()
                } else {
                        write_conn <- conn
                }

                temp_table <- make_temp_table_name()
                units_temp_table <- paste0(temp_table, "_units")

                pg13::dropTable(conn = write_conn,
                                schema = writeSchema,
                                tableName = temp_table)

                pg13::writeTable(conn = write_conn,
                                schema = writeSchema,
                                tableName = temp_table,
                                .data)

                if (is.null(conn)) {
                        dcAthena(conn = write_conn)
                }


                # # drug_strength with hours denominators
                # rate_concepts <-
                #         queryAthena(SqlRender::render(
                #                         "
                #                         SELECT DISTINCT c.*
                #                         FROM @vocabSchema.drug_strength ds
                #                         LEFT JOIN @vocabSchema.concept c
                #                         ON c.concept_id = ds.drug_concept_id
                #                         WHERE ds.denominator_unit_concept_id = 8505
                #                         ;
                #                         ", vocabSchema = vocabSchema),
                #                     conn = conn,
                #                     cache_only = FALSE,
                #                     skip_cache = TRUE,
                #                     override_cache = FALSE,
                #                     render_sql = FALSE,
                #                     verbose = FALSE,
                #                     sleepTime = 1)




                sendAthena(conn = conn,
                        SqlRender::render("

                        DROP TABLE IF EXISTS @writeSchema.@units_table;
                        CREATE TABLE  @writeSchema.@units_table (
                                drug_concept_id INTEGER,
                                unit_concept_id_type VARCHAR(255),
                                unit_concept_id INTEGER,
                                unit_concept_name VARCHAR(255)
                        );

                        WITH target_concepts AS (
                                                SELECT DISTINCT
                                                      a.*,
                                                      b.*
                                                FROM @writeSchema.@temp_table a
                                                LEFT JOIN @vocabSchema.drug_strength b
                                                ON a.@drug_concept_id_column = b.drug_concept_id
                                                WHERE b.invalid_reason IS NULL
                                        ),
                                        target_concept_units AS (
                                                SELECT
                                                        drug_concept_id,
                                                        unnest(array['amount_unit_concept_id', 'numerator_unit_concept_id', 'denominator_unit_concept_id']) AS unit_concept_id_type,
                                                       unnest(array[amount_unit_concept_id, numerator_unit_concept_id, denominator_unit_concept_id]) AS unit_concept_id
                                                FROM target_concepts
                                        ),
                                        target_concept_units2 AS (
                                                SELECT
                                                        u.*,
                                                        c.concept_name AS unit_concept_name
                                                FROM target_concept_units u
                                                LEFT JOIN @vocabSchema.concept c
                                                ON c.concept_id = u.unit_concept_id
                                        )

                                        INSERT INTO @writeSchema.@units_table
                                        SELECT *
                                        FROM target_concept_units2
                                        WHERE unit_concept_id IS NOT NULL
                                        ;",
                                          writeSchema = writeSchema,
                                          temp_table = temp_table,
                                          units_table = units_temp_table,
                                          drug_concept_id_column = drug_concept_id_column,
                                          vocabSchema = vocabSchema))


                target_unit_ids  <-
                        queryAthena(SqlRender::render(
                                        "SELECT DISTINCT *
                                        FROM crosstab('SELECT drug_concept_id, unit_concept_id_type, unit_concept_id FROM @writeSchema.@units_table') AS final_result(drug_concept_id INTEGER, amount_unit_concept_id INTEGER, numerator_unit_concept_id INTEGER, denominator_unit_concept_id INTEGER);",
                                        writeSchema = writeSchema,
                                        units_table = units_temp_table),
                                    conn = conn,
                                    cache_only = FALSE,
                                    skip_cache = TRUE,
                                    override_cache = FALSE,
                                    render_sql = FALSE,
                                    verbose = FALSE,
                                    sleepTime = 1)


                target_unit_names  <-
                        queryAthena(SqlRender::render(
                                "SELECT DISTINCT *
FROM crosstab('SELECT
					drug_concept_id,
					REPLACE(unit_concept_id_type, ''id'', ''name'') AS unit_concept_name_type,
					unit_concept_name
					FROM @writeSchema.@units_table') AS final_result(drug_concept_id INTEGER, amount_unit_concept_name VARCHAR, numerator_unit_concept_name VARCHAR, denominator_unit_concept_name VARCHAR)",
                                writeSchema = writeSchema,
                                units_table = units_temp_table),
                                conn = conn,
                                cache_only = FALSE,
                                skip_cache = TRUE,
                                override_cache = FALSE,
                                render_sql = FALSE,
                                verbose = FALSE,
                                sleepTime = 1)

                target_units <-
                        dplyr::left_join(target_unit_ids,
                                         target_unit_names,
                                         by = "drug_concept_id") %>%
                        dplyr::distinct()


                # Combine unit names
                target_units2 <-
                        target_units %>%
                        tidyr::unite(col = "staged_unit",
                                     numerator_unit_concept_name,
                                     denominator_unit_concept_name,
                                     sep = "/",
                                     remove = FALSE,
                                     na.rm = TRUE) %>%
                        # Unite on rows with amount_values instead of a numerator/denominator combo returns blank rather than NA so it is normalized back for a coalesce to get a complete staged unit representation
                        dplyr::mutate(staged_unit = dplyr::na_if(staged_unit, "")) %>%
                        dplyr::mutate(staged_unit = dplyr::coalesce(staged_unit, amount_unit_concept_name))


                target_concepts <-
                queryAthena(conn = conn,
                           SqlRender::render("WITH drug_strengths AS (
                                                SELECT DISTINCT
                                                      b.drug_concept_id,
                                                      b.ingredient_concept_id,
                                                      b.amount_value,
                                                      b.amount_unit_concept_id,
                                                      b.numerator_value,
                                                      b.numerator_unit_concept_id,
                                                      b.denominator_value,
                                                      b.denominator_unit_concept_id
                                                FROM @writeSchema.@temp_table a
                                                LEFT JOIN @vocabSchema.drug_strength b
                                                ON a.@drug_concept_id_column = b.drug_concept_id
                                                WHERE b.invalid_reason IS NULL
                           )
                                        SELECT DISTINCT *
                                        FROM @writeSchema.@temp_table c
                                        LEFT JOIN drug_strengths ds
                                        ON c.@drug_concept_id_column = ds.drug_concept_id
                                        ;",
                                             writeSchema = writeSchema,
                                             temp_table = temp_table,
                                             drug_concept_id_column = drug_concept_id_column,
                                             vocabSchema = vocabSchema))
                target_concepts %>%
                        dplyr::left_join(target_units2, by = c("drug_concept_id", "amount_unit_concept_id", "numerator_unit_concept_id", "denominator_unit_concept_id")) %>%
                        dplyr::distinct() %>%
                        tidyr::unite(col = "staged_value",
                                     numerator_value,
                                     denominator_value,
                                     sep = "/",
                                     na.rm = TRUE,
                                     remove = FALSE) %>%
                # Unite on rows with amount_values instead of a numerator/denominator combo returns blank rather than NA so it is normalized back for a coalesce to get a complete staged unit representation
                        dplyr::mutate(staged_value = dplyr::na_if(staged_value, "")) %>%
                                dplyr::mutate(staged_value = dplyr::coalesce(staged_value, as.character(amount_value))) %>%
                        dplyr::select(dplyr::all_of(drug_concept_id_column),
                                      drug_concept_id,
                                      ingredient_concept_id,
                                      staged_value,
                                      staged_unit,
                                      amount_value,
                                      amount_unit_concept_id,
                                      amount_unit_concept_name,
                                      numerator_value,
                                      numerator_unit_concept_id,
                                      numerator_unit_concept_name,
                                      denominator_value,
                                      denominator_unit_concept_id,
                                      denominator_unit_concept_name)

        }
