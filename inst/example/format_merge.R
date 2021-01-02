library(tidyverse)
library(chariot)

conn <- connectAthena()
# Get a strip for an aspirin concept
get_strip(concept_id = 1112807,
         vocab_schema = "omop_vocabulary",
         conn = conn)

# Unmerge the Strip to get each attribute back
test_strip <-
        tibble::tibble(
                concept = "[V] [S] 1112807 aspirin [RxNorm 1191] [Drug] [Ingredient]"
                )
unmerge_strip(test_strip,
              strip_col = concept)

# Merge a dataframe of concepts into stris
test_data <-
        queryAthena("SELECT * FROM omop_vocabulary.concept ORDER BY RANDOM() LIMIT 20;",
                    conn = conn)
merge_strip(test_data,
            into = "concept")


dcAthena(conn = conn)
