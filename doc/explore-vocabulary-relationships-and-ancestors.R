## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(chariot)

## -----------------------------------------------------------------------------
relationship_types <- 
  queryAthena(sql_statement = 
                "
                SELECT DISTINCT 
                  is_hierarchical,
                  defines_ancestry
                FROM omop_vocabulary.relationship
                ")
relationship_types

## -----------------------------------------------------------------------------
target_vocabularies <- c("SNOMED", "LOINC", 
                         "RxNorm", "RxNorm Extension", 
                         "HemOnc", "ATC")

## ----cache=TRUE---------------------------------------------------------------
output <- list()
for (i in seq_along(target_vocabularies)) {
  target_vocabulary <- target_vocabularies[i]
  sql_statement <- 
    SqlRender::render(
      "
      SELECT DISTINCT
        c.vocabulary_id,
        c.concept_class_id AS concept_class_id_1,
        c2.concept_class_id AS concept_class_id_2,
        r.is_hierarchical,
        r.defines_ancestry
      FROM omop_vocabulary.concept c 
      INNER JOIN omop_vocabulary.concept_relationship cr 
      ON cr.concept_id_1 = c.concept_id 
      INNER JOIN omop_vocabulary.concept c2 
      ON cr.concept_id_2 = c2.concept_id
      LEFT JOIN omop_vocabulary.relationship r 
      ON r.relationship_id = cr.relationship_id
      WHERE 
        c.invalid_reason IS NULL
          AND c.vocabulary_id IN ('@target_vocabulary') 
          AND c2.invalid_reason IS NULL 
          AND c2.vocabulary_id IN ('@target_vocabulary')
          AND cr.invalid_reason IS NULL;
      ",
      target_vocabulary = target_vocabulary
    )
  output[[i]] <-
    queryAthena(
      sql_statement = sql_statement
    )
}

