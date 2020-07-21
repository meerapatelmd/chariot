SELECT c.*
FROM @schema.concept_relationship cr
LEFT JOIN @schema.concept c
ON c.concept_id = cr.concept_id_2
WHERE cr.concept_id_1 IN (@component_concept_ids)
AND cr.relationship_id = 'Antineoplastic of'
AND c.concept_class_id = 'Regimen'
AND c.vocabulary_id = 'HemOnc';
