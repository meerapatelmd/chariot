## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup,message=FALSE,warning=FALSE,results='hide'-------------------------
library(chariot)
library(tidyverse)

## ----results='hide', message=FALSE--------------------------------------------
conn <- connectAthena()

## -----------------------------------------------------------------------------
test_data <- 
  get_test_drug_concepts(conn = conn)

## -----------------------------------------------------------------------------
test_data

## -----------------------------------------------------------------------------
test_data_relationships <- lookup_relationships(test_data$concept_id,
                                                conn = conn)

## -----------------------------------------------------------------------------
test_data_relationships

## -----------------------------------------------------------------------------
test_data_relationships2 <-
  test_data_relationships %>%
  merge_strip(into = "concept_1", 
              suffix = "_1") %>%
  merge_strip(into = "concept_2",
              suffix = "_2")

## -----------------------------------------------------------------------------
test_data_relationships2

## -----------------------------------------------------------------------------
output <-
  test_data_relationships2 %>%
  pivot_wider(id_cols = concept_1,
              names_from = relationship_id, 
              values_from = concept_2)
output

## -----------------------------------------------------------------------------
output <-
  test_data_relationships2 %>%
  pivot_wider(id_cols = concept_1,
              names_from = relationship_id, 
              values_from = concept_2,
              # Add aggregate function
              values_fn = list(concept_2 = ~ paste(unique(.), collapse = "|"))
              )
output

## ----include=FALSE------------------------------------------------------------
dcAthena(conn = conn)

