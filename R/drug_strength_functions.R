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


#' @title
#' Stage Drug Strength Expressionsfor Evaluation
#' @description
#' The Drug Strength Table is joined to the provided drug_concept_ids and the amount_value or fraction of numerator to denominator values are returned in a "staged_value" field along with the concept names of the units in a seprate "staged_unit" field. Only valid Drug Strength Table entries are included in the returned dataframe and the 'box_size', 'invalid_reason', 'valid_start_date', and 'valid_end_date' fields are excluded in the output. Since some extended release formulations of common medications such as Tylenol have a a Drug Strength with a denominator of 24 hours, the `include_rates` parameter denotes whether those rate-based calculations should be included in the staged value. If FALSE, all the denominator and subsequent staged values are calculated in their native form and prefixed with 'native_' and then the final staged and denominator values are reconstituted with denominator units related to time measurements normalized to 1.
#' @param .data PARAM_DESCRIPTION
#' @param drug_concept_id_column PARAM_DESCRIPTION
#' @param include_rates PARAM_DESCRIPTION, Default: FALSE
#' @param conn PARAM_DESCRIPTION, Default: NULL
#' @param vocabSchema PARAM_DESCRIPTION, Default: 'omop_vocabulary'
#' @param writeSchema PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details
#' The staged_value and/or native_staged_value fields are returned as character class expressions to evaluate to limit rounding error in downstream calculations for non-whole number drug strengths. For example, the staged_value for a drug with a numerator of 1 and denominator of 3 would have a staged_value of "1/3" rather than 0.3333. These values can be parsed into their numeric values by calling a function that parses and evaluates the expression such as the rlang's parse_expr function followed by a call to the base eval function.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[pg13]{dropTable}},\code{\link[pg13]{writeTable}}
#'  \code{\link[SqlRender]{render}}
#'  \code{\link[dplyr]{mutate-joins}},\code{\link[dplyr]{distinct}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{na_if}},\code{\link[dplyr]{coalesce}},\code{\link[dplyr]{select}},\code{\link[dplyr]{reexports}},\code{\link[dplyr]{rename}}
#'  \code{\link[tidyr]{unite}}
#'  \code{\link[tibble]{tribble}}
#' @rdname stage_drug_strength_formulas
#' @export
#' @importFrom pg13 dropTable writeTable
#' @importFrom SqlRender render
#' @importFrom dplyr left_join distinct mutate na_if coalesce select all_of rename
#' @importFrom tidyr unite
#' @importFrom tibble tribble


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


                target_concepts2 <-
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


                if (include_rates) {

                        pg13::dropTable(conn = write_conn,
                                        schema = writeSchema,
                                        tableName = temp_table)

                        pg13::dropTable(conn = write_conn,
                                        schema = writeSchema,
                                        tableName = units_temp_table)

                        if (is.null(conn)) {
                                dcAthena(conn = write_conn)
                        }

                        return(target_concepts2)

                } else {
                        rate_units <-
                                tibble::tribble(
                                        ~rate_concept_id,~rate_concept_name,
                                        "8483", "counts per minute",
                                        "8505", "hour",
                                        "8511", "week",
                                        "8512", "day",
                                        "8541", "per minute",
                                        "8550", "minute",
                                        "8555", "second",
                                        "8621", "day per week",
                                        "8630", "unit per hour",
                                        "8631", "sperm motility at 60 minutes",
                                        "8698", "liter per minute",
                                        "8752", "millimeter per hour",
                                        "8774", "microgram per minute",
                                        "8791", "gram per 5 hours",
                                        "8795", "milliliter per minute",
                                        "8800", "percent at 60 minutes",
                                        "8807", "gram per 24 hours",
                                        "8857", "liter per 24 hours",
                                        "8906", "microgram per 24 hours",
                                        "8907", "micromole per 24 hours",
                                        "8908", "milligram per 12 hours",
                                        "8909", "milligram per 24 hours",
                                        "8910", "millimole per 24 hours",
                                        "8928", "microequivalent per 24 hours",
                                        "8930", "milliliter per 24 hours",
                                        "8955", "nanomole per second and liter",
                                        "8992", "nanogram per deciliter per hour",
                                        "8993", "nanomole per hour and milligram",
                                        "9020", "nanogram per milliliter and hour",
                                        "9117", "milliliter per minute per 1.73 square meter",
                                        "9145", "nanogram per hour per milligram of total protein",
                                        "9211", "minute",
                                        "9212", "second",
                                        "9215", "percent 0 to 3 hours",
                                        "9355", "light-year",
                                        "9448", "year",
                                        "9449", "mean gregorian year",
                                        "9450", "mean julian year",
                                        "9451", "tropical year",
                                        "9537", "kilosecond",
                                        "9541", "liter-square second per second",
                                        "9580", "month",
                                        "9581", "mean gregorian month",
                                        "9582", "mean julian month",
                                        "9583", "synodal month",
                                        "9592", "megasecond",
                                        "9593", "millisecond",
                                        "9616", "nanosecond",
                                        "9634", "picosecond",
                                        "9672", "micromole per minute and gram",
                                        "9676", "microsecond",
                                        "9687", "international unit per hour",
                                        "9688", "microgram per kilogram per minute",
                                        "9690", "microgram per kilogram per hour",
                                        "9691", "milligram per kilogram per hour",
                                        "9692", "milligram per kilogram per minute",
                                        "32699", "milliliter per minute per millimeter mercury column",
                                        "32700", "liter per second",
                                        "32703", "dyne-second per centimeter to the fifth power",
                                        "32704", "millimeter mercury column-minute per liter",
                                        "32710", "liter per minute per square meter",
                                        "32738", "centimeter per second",
                                        "44777556", "per 24 hours",
                                        "44777557", "per hour",
                                        "44777559", "per week",
                                        "44777560", "per year",
                                        "44777564", "international unit per 24 hours",
                                        "44777567", "unit per 24 hours",
                                        "44777582", "milliequivalent per 24 hours",
                                        "44777593", "gram per 12 hours",
                                        "44777594", "gram per hour",
                                        "44777603", "liter per hour",
                                        "44777606", "meter per second",
                                        "44777607", "meter per square second",
                                        "44777610", "milligram per hour",
                                        "44777611", "milligram per minute",
                                        "44777613", "milliliter per hour",
                                        "44777614", "milliliter per second",
                                        "44777621", "mole per second",
                                        "44777622", "millipascal second",
                                        "44777624", "nanogram per 24 hours",
                                        "44777627", "nanomole per 24 hours",
                                        "44777629", "nanomole per hour and gram",
                                        "44777630", "nanomole per hour and liter",
                                        "44777631", "nanomole per hour and milliliter",
                                        "44777632", "nanomole per hour and milliliter red blood cells",
                                        "44777635", "nanomole per minute and milliliter",
                                        "44777637", "nanomole per second",
                                        "44777640", "picomole per 24 hours",
                                        "44777642", "picomole per hour and milligram of hemoglobin",
                                        "44777645", "microgram per hour",
                                        "44777650", "micromole per hour and gram of hemoglobin",
                                        "44777651", "micromole per hour and gram of protein",
                                        "44777652", "micromole per hour and liter",
                                        "44777653", "micromole per hour and milliliter",
                                        "44777655", "micromole per minute",
                                        "44777658", "per month",
                                        "44777659", "per second",
                                        "44777664", "milliliter per 12 hours",
                                        "45891004", "month supply",
                                        "45891005", "week supply",
                                        "45891021", "milligram per 16 hours",
                                        "45891022", "milligram per 72 hours",
                                        "45891023", "microgram per 72 hours") %>%
                                dplyr::mutate(rate_concept_id = as.integer(rate_concept_id))


                        target_concepts3 <-
                        target_concepts2 %>%
                                dplyr::left_join(rate_units,
                                                 by = c("denominator_unit_concept_id" = "rate_concept_id")) %>%
                                dplyr::rename(native_denominator_value = denominator_value,
                                              native_denominator_unit_concept_id = denominator_unit_concept_id,
                                              native_denominator_unit_concept_name = denominator_unit_concept_name) %>%
                                dplyr::mutate(denominator_value = ifelse(!is.na(rate_concept_name), NA_character_, native_denominator_value),
                                              denominator_unit_concept_id = ifelse(!is.na(rate_concept_name), NA_character_, native_denominator_unit_concept_id),
                                              denominator_unit_concept_name = ifelse(!is.na(rate_concept_name), NA_character_, native_denominator_unit_concept_id)) %>%
                                dplyr::rename(native_staged_value = staged_value,
                                              native_staged_unit = staged_unit) %>%
                                tidyr::unite(col = "staged_unit",
                                             numerator_unit_concept_name,
                                             denominator_unit_concept_name,
                                             sep = "/",
                                             remove = FALSE,
                                             na.rm = TRUE) %>%
                                # Unite on rows with amount_values instead of a numerator/denominator combo returns blank rather than NA so it is normalized back for a coalesce to get a complete staged unit representation
                                dplyr::mutate(staged_unit = dplyr::na_if(staged_unit, "")) %>%
                                dplyr::mutate(staged_unit = dplyr::coalesce(staged_unit, amount_unit_concept_name))  %>%
                                tidyr::unite(col = "staged_value",
                                             numerator_value,
                                             denominator_value,
                                             sep = "/",
                                             na.rm = TRUE,
                                             remove = FALSE) %>%
                                # Unite on rows with amount_values instead of a numerator/denominator combo returns blank rather than NA so it is normalized back for a coalesce to get a complete staged unit representation
                                dplyr::mutate(staged_value = dplyr::na_if(staged_value, "")) %>%
                                dplyr::mutate(staged_value = dplyr::coalesce(staged_value, as.character(amount_value))) %>%
                                dplyr::select(
                                                dplyr::all_of(drug_concept_id_column),
                                                        drug_concept_id,
                                                        ingredient_concept_id,
                                                        staged_value,
                                                        staged_unit,
                                                        native_staged_value,
                                                        native_staged_unit,
                                                        amount_value,
                                                        amount_unit_concept_id,
                                                        amount_unit_concept_name,
                                                        numerator_value,
                                                        numerator_unit_concept_id,
                                                        numerator_unit_concept_name,
                                                        denominator_value,
                                                        denominator_unit_concept_id,
                                                        denominator_unit_concept_name,
                                                        native_denominator_value,
                                                        native_denominator_unit_concept_id,
                                                        native_denominator_unit_concept_name)


                        pg13::dropTable(conn = write_conn,
                                        schema = writeSchema,
                                        tableName = temp_table)

                        pg13::dropTable(conn = write_conn,
                                        schema = writeSchema,
                                        tableName = units_temp_table)

                        if (is.null(conn)) {
                                dcAthena(conn = write_conn)
                        }

                        return(target_concepts3)


                }

        }
