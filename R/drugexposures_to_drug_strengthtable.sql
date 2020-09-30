DROP TABLE IF EXISTS patelm9.drug_exposure_to_strength;
CREATE TABLE patelm9.drug_exposure_to_strength (
    concept_name text,
    drug_type_concept_id integer,
    refills integer,
    quantity double precision,
    days_supply integer,
    sig text,
    route_concept_id integer,
    drug_concept_id integer,     
    ingredient_concept_id integer,
    amount_value numeric,
    amount_unit_concept_id integer,
    numerator_value numeric,
    numerator_unit_concept_id integer,
    denominator_value numeric,
    denominator_unit_concept_id integer,
    box_size integer,
    valid_start_date date,
    valid_end_date date,
    invalid_reason character varying(1),
    CONSTRAINT xpk_drug_strength PRIMARY KEY (drug_concept_id, ingredient_concept_id)
    )
;

            WITH de_counts AS (
                        SELECT drug_concept_id,COUNT(drug_concept_id)
                        FROM omop_cdm_2.drug_exposure de
                        GROUP BY drug_concept_id
            ),
            distinct_de AS (
                    SELECT DISTINCT
                        de.drug_concept_id,
                        c.count,
                        de.refills,
                        de.quantity,
                        de.days_supply,
                        de.sig,
                        de.route_concept_id
                    FROM de_counts c
                    LEFT JOIN omop_cdm_2.drug_exposure de
                    ON c.drug_concept_id = de.drug_concept_id
            ),
            distinct_de_concept AS (
                    SELECT dde.*, c.concept_name
                    FROM distinct_de dde
                    LEFT JOIN omop_vocabulary.concept c
                    ON c.concept_id = dde.drug_concept_id
            ),
            CREATE TABLE patelm9.drug_exposure_to_strength (
            SELECT
                ddc.concept_name,
                ddc.refills,
                ddc.quantity,
                ddc.days_supply,
                ddc.sig,
                ddc.route_concept_id,
                ds.*
            FROM distinct_de_concept ddc
            LEFT JOIN omop_vocabulary.drug_strength ds
            ON ds.drug_concept_id = ddc.drug_concept_id
            );  