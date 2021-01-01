library(tidyverse)
library(chariot)

# Search the OMOP Vocabulary for an exact match to the phrase
# "Myocardial Infarction" insensitive to case.
search_exact_phrase(phrase = "Myocardial Infarction",
                    case_insensitive = TRUE)


# Repeat with case sensitivity
search_exact_phrase(phrase = "Myocardial Infarction",
                    case_insensitive = FALSE)


# Search concepts that contain the phrase "Myocardial Infarction"
search_like_phrase(phrase = "Myocardial infarction")
search_like_phrase(phrase = "Myocardial infarction",
                   case_insensitive = FALSE)

# Search concepts that start with or end with the phrase "Myocardial Infarction"
search_starts_with_phrase(phrase = "Myocardial infarction")
search_ends_with_phrase(phrase = "Myocardial infarction")

# Search a phrase by splitting into smaller words that are each included in a
# 'LIKE' clause
search_split_phrase(phrase = "Myocardial infarction",
                    split = " ")
