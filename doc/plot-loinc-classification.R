## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = "hide"
)

## ----setup--------------------------------------------------------------------
library(chariot)

## -----------------------------------------------------------------------------
loinc_class_concept <- get_concept(37060596,
                          vocab_schema = "omop_vocabulary")

## -----------------------------------------------------------------------------
loinc_class_concept

