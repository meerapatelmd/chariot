## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup,message=FALSE,warning=FALSE,results='hide'-------------------------
library(chariot)
library(tidyverse)

## ----results='hide', message=FALSE--------------------------------------------
conn <- connectAthena()

## -----------------------------------------------------------------------------
test_data <- 
  get_test_drug_concepts(conn = conn,
                         limit = 1)

## -----------------------------------------------------------------------------
test_data

## -----------------------------------------------------------------------------
test_data_ancestors <-
  join_for_ancestors(data = test_data,
                     descendant_id_column = "concept_id",
                     conn = conn)

## -----------------------------------------------------------------------------
test_data_ancestors

## -----------------------------------------------------------------------------
test_data_descendants <-
  join_for_descendants(
    data = test_data, 
    ancestor_id_column = "concept_id",
    conn = conn
  )

## -----------------------------------------------------------------------------
test_data_descendants

## -----------------------------------------------------------------------------
test_data_ancestors2 <- 
  test_data_ancestors %>%
  rename_all(str_replace_all, "ancestor_", "relative_") %>% 
  mutate(relative_type = "ancestor") %>%
  arrange(desc(min_levels_of_separation))

test_data_descendants2 <-
  test_data_descendants %>%
  rename_all(str_replace_all, "descendant_", "relative_") %>% 
  mutate(relative_type = "descendant") %>%
  arrange(min_levels_of_separation)

test_data_relatives <-
  bind_rows(test_data_ancestors2,
            test_data_descendants2)

test_data_relatives

## -----------------------------------------------------------------------------
test_data_relatives2 <-
  test_data_relatives %>%
  unite(col = levels_of_separation, 
        relative_type,
        min_levels_of_separation) %>%
  mutate(levels_of_separation = factor(levels_of_separation,
                                       levels = c("ancestor_3", "ancestor_2",
                                                  "ancestor_1", "ancestor_0", 
                                                  "descendant_0", "descendant_1")))
test_data_relatives2$levels_of_separation

## -----------------------------------------------------------------------------
test_data_relatives3 <-
  test_data_relatives2 %>%
  merge_strip(into = "concept") %>%
  merge_strip(into = "relative", 
              prefix = "relative_")

## -----------------------------------------------------------------------------
test_data_relatives3

## -----------------------------------------------------------------------------
output <-
  test_data_relatives3 %>%
  pivot_wider(id_col = concept,
              names_from = levels_of_separation, 
              values_from = relative)
output

## -----------------------------------------------------------------------------
output <-
  test_data_relatives3 %>%
  pivot_wider(id_col = concept,
              names_from = levels_of_separation, 
              values_from = relative,
              #Add aggregate 
              values_fn = list(relative = ~ paste(unique(.), collapse = "|")))
output

## ----include=FALSE------------------------------------------------------------
dcAthena(conn = conn)

