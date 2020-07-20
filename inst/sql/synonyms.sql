SELECT *
FROM @schema.concept_synonym
WHERE concept_id = @concept_id
        AND language_concept_id = @language_concept_id;
