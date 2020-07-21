SELECT DISTINCT
        c.concept_class_id AS @concept_class_id_1,
        cr.relationship_id,
        c2.concept_class_id AS @concept_class_id_2
FROM @schema.concept c
LEFT JOIN @schema.concept_relationship cr
ON cr.concept_id_1 = c.concept_id
LEFT JOIN @schema.concept c2
ON c2.concept_id = cr.concept_id_2
WHERE c.vocabulary_id IN (@vocabulary_id_1)
AND c2.vocabulary_id IN (@vocabulary_id_2);
