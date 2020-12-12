library(tidyverse)
library(chariot)

get_strip(concept_id = 1112807,
         vocab_schema = "omop_vocabulary")

test_strip <- tibble::tibble(concept = "[V] [S] 1112807 aspirin [RxNorm 1191] [Drug] [Ingredient]")
unmerge_strip(test_strip,
              strip_col = concept)

test_data <-
        queryAthena("SELECT * FROM omop_vocabulary.concept ORDER BY RANDOM() LIMIT 20;",
                    cache_only = TRUE)
merge_strip(test_data,
            into = "concept")
