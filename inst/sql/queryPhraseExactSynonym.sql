SELECT c.*, STRING_AGG(cs.concept_synonym_name, '|') AS concept_synonym_name
FROM @schema.concept_synonym cs
LEFT JOIN @schema.concept c
ON c.concept_id = cs.concept_id
WHERE cs.concept_synonym_name IN (@phrase)
GROUP BY cs.concept_id;
