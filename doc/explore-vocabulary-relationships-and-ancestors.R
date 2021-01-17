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

