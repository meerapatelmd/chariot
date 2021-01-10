library(chariot)
library(tidyverse)

# Get Unfiltered Test Concepts (Invalid Concepts are included)
get_test_concepts()

# The size of the resultset can be altered
get_test_concepts(limit = 25)

# Concepts specific to the Drug domain can be sampled
# RxNorm/RxNorm Extension Concepts
get_test_drug_concepts(limit = 10)

# HemOnc/ATC Classes
get_test_drug_classes(limit = 10)

# Test data can also be taken from tables other than Concept
get_test_data(table = "concept_relationship")
get_test_data(table = "concept_ancestor", limit = 10)
