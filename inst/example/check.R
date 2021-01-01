library(chariot)
library(tidyverse)

# Checking concept id
check_concept_id(concept_id = 2160178)

# Doesn't matter if it is a real concept id or not. It only checks if it is a valid
# integer
check_concept_id(concept_id = 216017800)

# Examples of concept ids that would be invalid
check_concept_id(concept_id = "216017800A")


# Check that a concept belongs to the expected vocabulary.
# If the Concept Id does not exist in the Concept table, an alert is returned
check_vocab_id(concept_obj = 2160178,
               vocabulary_id = "HemOnc")

check_vocab_id(concept_obj = 2160178,
               vocabulary_id = "ATC")

# If the Concept Id exists in the Concept table and the vocabulary_id is wrong
check_vocab_id(concept_obj = 1308216,
               vocabulary_id = "ATC")

# If the Concept Id exists in the Concept table and the vocabulary_id is correct
check_vocab_id(concept_obj = 1308216,
               vocabulary_id = "RxNorm")

