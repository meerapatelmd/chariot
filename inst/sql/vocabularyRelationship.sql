SELECT c1.*,cr1.relationship_id,cr1.concept_id_2,cr1.invalid_reason as cr_invalid_reason
FROM @schema.concept c1
LEFT JOIN @schema.concept_relationship cr1
ON c1.concept_id = cr1.concept_id_1
WHERE c1.vocabulary_id = '@vocabulary_id';
