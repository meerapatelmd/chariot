library(tidyverse)
library(chariot)

getStrip(concept_id = 1112807,
         schema = "omop_vocabulary")


test_data <-
    tibble::tibble(Concept = "[V] [S] 1112807 aspirin [RxNorm 1191] [Drug] [Ingredient]\n[V] [S] 1112807 aspirin [RxNorm 1191] [Drug] [Ingredient]")

unboxStrip(data = test_data,
           strip_col = Concept)
unboxStrip(data = test_data,
           strip_col = Concept,
           remove = TRUE)
