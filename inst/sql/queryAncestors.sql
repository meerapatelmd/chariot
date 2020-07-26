SELECT *
FROM concept_ancestor a
LEFT JOIN concept
ON concept_id = a.ancestor_concept_id
WHERE a.descendant_concept_id IN (@descendant_concept_ids);
