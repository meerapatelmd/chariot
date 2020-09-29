#' @title
#' Drug Strength Table Functions
#'
#' @name drug_strength_table_functions
#' @keywords internal
NULL



TEST_DATA <-
pg13::query(conn = conn,
            "WITH de_counts AS (
                        SELECT drug_concept_id,COUNT(drug_concept_id)
                        FROM omop_cdm_2.drug_exposure de
            ),
            distinct_de AS (
                    SELECT DISTINCT
                        de.drug_concept_id,
                        de.refills,
                        de.quantity,
                        de.days_supply,
                        de.sig
                    FROM de_counts c
                    LEFT JOIN omop_cdm_2.drug_exposure de
                    ON c.drug_concept_id = de.drug_concept_id
            ),
            distinct_de_concept AS (
                    SELECT dde.*, c.concept_name
                    FROM distinct_de dde
                    LEFT JOIN omop_vocabulary.concept c
                    ON c.concept_id = dde.drug_concept_id
            )

            SELECT
                ddc.concept_name,
                ddc.refills,
                ddc.quantity,
                ddc.days_supply,
                ddc.sig,
                ds.*
            FROM distinct_de_concept ddc
            LEFT JOIN omop_vocabulary.drug_strength ds
            ON ds.drug_concept_id = ddc.drug_concept_id")


pg13::query(conn = conn,
            "SELECT DISTINCT
                c.vocabulary_id, c.concept_class_id
            FROM DRUG_STRENGTH ds
            LEFT JOIN CONCEPT c
            ON c.concept_id = ds.ingredient_concept_id
            WHERE c.invalid_reason IS NULL")
