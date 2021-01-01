library(tidyverse)
library(chariot)

lookup_atc_class_ingredients(atc_concept_obj = 21601787)

lisinopril_class <- get_concept(concept_id = 21601787)
lisinopril_class

lookup_atc_class_ingredients(atc_concept_obj = lisinopril_class)
