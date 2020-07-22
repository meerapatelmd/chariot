SELECT cr.concept_id_1 AS regimen_concept_id,
       c.concept_id AS has_antineoplastic_concept_id,
       c.concept_name AS has_antineoplastic_concept_name
FROM @schema.concept_relationship cr
LEFT JOIN @schema.concept c
ON c.concept_id = cr.concept_id_2
WHERE cr.concept_id_1 IN (@regimen_concept_ids)
AND cr.relationship_id = 'Has antineoplastic'
AND c.concept_class_id = 'Component'
AND c.vocabulary_id = 'HemOnc';
