library(tidyverse)
library(chariot)

get_strip(concept_id = 1112807,
         vocab_schema = "omop_vocabulary")

test_strip <- tibble::tibble(concept = "[V] [S] 1112807 aspirin [RxNorm 1191] [Drug] [Ingredient]")
unmerge_strip(test_strip,
              strip_col = concept)

unbox_strip(test_strip,
            strip_col = concept)

test_strip <- tibble::tibble(concept = "[V] [S] 1112807 aspirin [RxNorm 1191] [Drug] [Ingredient]\n[V] [S] 1112807 aspirin [RxNorm 1191] [Drug] [Ingredient]")
unbox_strip(test_strip,
            strip_col = concept)

