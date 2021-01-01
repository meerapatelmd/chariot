library(chariot)
library(tidyverse)

concept_obj <- get_concept(concept_id = 1308216)
print_concept_hierarchy(concept_obj = concept_obj)
print_concept_hierarchy(concept_obj = concept_obj,
                        level_of_separation_type = "min")


concept_obj <- get_concept(concept_id = 4187868)
print_concept_hierarchy(concept_obj = concept_obj)
print_concept_hierarchy(concept_obj = concept_obj,
                        level_of_separation_type = "min")
