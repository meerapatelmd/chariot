## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = "hide"
)

## ----setup--------------------------------------------------------------------
library(chariot)
conn <- chariot::connectAthena()

## ----concept_class_obj--------------------------------------------------------
loinc_class_concept <- get_concept(37052681,
                          vocab_schema = "omop_vocabulary",
                          conn = conn)

## ----concept_class_obj2-------------------------------------------------------
loinc_class_concept

## ----preview------------------------------------------------------------------
preview_loinc_classification(concept_class_obj = loinc_class_concept,
                     conn = conn)

## ----plot---------------------------------------------------------------------
temp_html <- tempfile(fileext = ".html")
plot_loinc_classification(conn = conn,
                          concept_class_obj = loinc_class_concept,
                          range = 1:2,
                          file = temp_html)

## ----display------------------------------------------------------------------
htmltools::includeHTML(path = temp_html)

## ----final--------------------------------------------------------------------
unlink(temp_html)
dcAthena(conn = conn)

