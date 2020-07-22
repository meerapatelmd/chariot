SELECT cr.concept_id_1 AS component_concept_id,
       c.concept_id AS regimen_concept_id,
       c.concept_name AS regimen_concept_name,
       cr2.concept_id_2 AS has_antineoplastic_concept_id
--First Join to CONCEPT_RELATIONSHIP to get all Input Components 'Antineoplastic of' relationships
FROM @schema.concept_relationship cr
LEFT JOIN @schema.concept c
ON c.concept_id = cr.concept_id_2
--Second Join to CONCEPT_RELATIONSHIP to get HemOnc Regimens that input maps to and 'Has antineoplastic (HemOnc)' relationships
LEFT JOIN @schema.concept_relationship cr2
ON cr2.concept_id_1 = c.concept_id
WHERE cr.concept_id_1 IN (@component_concept_ids)
AND cr.relationship_id = 'Antineoplastic of'
AND cr2.relationship_id = 'Has antineoplastic'
AND c.concept_class_id = 'Regimen'
AND c.vocabulary_id = 'HemOnc';
