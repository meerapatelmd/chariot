library(chariot)
library(tidyverse)

# Lookup relationships on the Vocabulary-Concept Class Id axis
vocab_lookup_relationships(vocabulary_id = "HemOnc")

# Relationships between HemOnc concepts only can also be looked up
vocab_lookup_intrarelation(vocabulary_id = "HemOnc")

# Relationships between HemOnc and non-Hemonc Concepts
vocab_lookup_interrelation(vocabulary_id = "HemOnc")
