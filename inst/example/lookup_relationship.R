test_data <-
        queryAthena(sql_statement =
                        "
                        SELECT *
                        FROM omop_vocabulary.concept c
                        WHERE c.invalid_reason IS NULL
                                AND c.vocabulary_id = 'HemOnc'
                                AND c.concept_class_id = 'Regimen'
                        ORDER BY RANDOM()
                        LIMIT 5;",
                    conn = conn)


output <-
lookup_relationship(concept_ids = test_data$concept_id,
                    conn = conn,
                    vocabSchema = "omop_vocabulary")
