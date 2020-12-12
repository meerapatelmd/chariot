## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = "hide"
)

## ----setup--------------------------------------------------------------------
library(chariot)
library(tidyverse)
conn <- chariot::connectAthena()

## ----concept_class_obj--------------------------------------------------------
atc_class_concept <- get_concept(21601386,
                          vocab_schema = "omop_vocabulary",
                          conn = conn)

## ----concept_class_obj2-------------------------------------------------------
atc_class_concept

## ----preview------------------------------------------------------------------
preview_atc_classification(concept_class_obj = atc_class_concept,
                     conn = conn)

## ----plot---------------------------------------------------------------------
temp_html <- tempfile(fileext = ".html")
plot_atc_classification(conn = conn,
                          concept_class_obj = atc_class_concept,
                          range = 1:5,
                          file = temp_html)

## ----display------------------------------------------------------------------
htmltools::includeHTML(path = temp_html)

## ----final--------------------------------------------------------------------
unlink(temp_html)
dcAthena(conn = conn)

