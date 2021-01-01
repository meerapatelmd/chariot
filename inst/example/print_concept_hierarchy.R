library(chariot)
library(tidyverse)

conn <- connectAthena()
# Drug Domain
concept_obj <- get_concept(concept_id = 1308216)
concept_obj
print_concept_hierarchy(conn = conn,
                        concept_obj = concept_obj)
print_concept_hierarchy(conn = conn,
                        concept_obj = concept_obj,
                        level_of_separation_type = "min")


# Cancer Condition Domain
concept_obj <- get_concept(concept_id = 4187868)
concept_obj
print_concept_hierarchy(conn = conn,
                        concept_obj = concept_obj)
print_concept_hierarchy(conn = conn,
                        concept_obj = concept_obj,
                        level_of_separation_type = "min")

# Chronic Disease Domain
concept_obj <- get_concept(concept_id = 319835)
concept_obj
print_concept_hierarchy(conn = conn,
                        concept_obj = concept_obj)
print_concept_hierarchy(conn = conn,
                        concept_obj = concept_obj,
                        level_of_separation_type = "min")

# Measurement Domain
concept_obj <- get_concept(concept_id = 4298431)
concept_obj
print_concept_hierarchy(conn = conn,
                        concept_obj = concept_obj)
print_concept_hierarchy(conn = conn,
                        concept_obj = concept_obj,
                        level_of_separation_type = "min")
