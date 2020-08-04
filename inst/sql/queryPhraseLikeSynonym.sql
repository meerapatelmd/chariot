SELECT c.*, STRING_AGG(DISTINCT cs.concept_synonym_name, '|') AS concept_synonym_name
FROM @schema.concept_synonym cs
LEFT JOIN @schema.concept c
ON c.concept_id = cs.concept_id AND c.concept_name <> cs.concept_synonym_name
WHERE cs.concept_synonym_name LIKE '%@phrase%'
GROUP BY c.concept_id;
