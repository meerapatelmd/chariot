library(chariot)
library(tidyverse)

conn <- connectAthena()


# Drug Domain
concept_obj <- get_concept(conn = conn,
                           concept_id = 1308216)
concept_obj
get_name_cluster(conn = conn,
                 concept_obj = concept_obj)


# Cancer Condition Domain
concept_obj <- get_concept(conn = conn,
                           concept_id = 4187868)
concept_obj
get_name_cluster(conn = conn,
                 concept_obj = concept_obj)

# Chronic Disease Domain
concept_obj <- get_concept(conn = conn,
                           concept_id = 319835)
concept_obj
get_name_cluster(conn = conn,
                 concept_obj = concept_obj)

# Measurement Domain
concept_obj <- get_concept(conn = conn, concept_id = 4298431)
concept_obj
get_name_cluster(conn = conn,
                 concept_obj = concept_obj)


dcAthena(conn = conn)
