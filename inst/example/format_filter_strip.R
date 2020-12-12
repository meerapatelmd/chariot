library(tidyverse)
library(chariot)

test_data <-
        queryAthena("SELECT * FROM omop_vocabulary.concept ORDER BY RANDOM() LIMIT 20;",
                    cache_only = TRUE)

merge_strip(data = test_data,
            into = "concept")

merge_strip(data = test_data,
            into = "concept",
            vocabulary_id,
            concept_class_id)

test_output <- merge_strip(data = test_data,
                        into = "concept") %>%
                dplyr::select(-concept_id)

# Filter at a Strip Column
filter_strip(test_output,
             strip_col = concept,
             vocabulary_id %in% c("RxNorm"))

# Filter at more than 1 Strip Column
test_data <-
        queryAthena("SELECT * FROM omop_vocabulary.concept ORDER BY RANDOM() LIMIT 10;",
                    cache_only = TRUE) %>%
        merge_strip(into = "concept")

test_data <-
        dplyr::bind_cols(test_data %>%
                                 rubix::randomize() %>%
                                 dplyr::select(concept_a = concept),
                         test_data %>%
                                 rubix::randomize() %>%
                                 dplyr::select(concept_b = concept))

filter_at_all_strip(data = test_data,
                strip_cols = c("concept_a",
                               "concept_b"),
                all = TRUE,
                vocabulary_id %in% c("RxNorm"))

filter_at_any_strip(data = test_data,
                    strip_cols = c("concept_a",
                                   "concept_b"),
                    all = TRUE,
                    vocabulary_id %in% c("RxNorm"))