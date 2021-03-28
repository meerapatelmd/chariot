## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup,message=FALSE------------------------------------------------------
library(chariot)
library(tidyverse)

## ----results='hide',message=FALSE---------------------------------------------
FOLFOX <- get_concept(concept_id = 35806596)
Irinotecan <- get_concept(concept_id = 35803130)

## ----results='hide',message=FALSE---------------------------------------------
new_components <- 
  ho_reduce_to_components(FOLFOX,
                        Irinotecan)

## -----------------------------------------------------------------------------
new_components

## ----results='hide',message=FALSE---------------------------------------------
new_regimen <- 
  ho_lookup_regimen(new_components$concept_id)

## -----------------------------------------------------------------------------
new_regimen

## ----results='hide',message=F-------------------------------------------------
Trastuzumab <- get_concept(concept_id = 35803361)
Trastuzumab

## -----------------------------------------------------------------------------
new_component_ids2 <- c(new_components$concept_id, Trastuzumab@concept_id)
new_component_ids2

## ----results='hide',message=FALSE---------------------------------------------
new_regimen2 <- ho_lookup_regimen(new_component_ids2)

## -----------------------------------------------------------------------------
new_regimen2

## ----results='hide',message=F-------------------------------------------------
new_component_objs2 <- lapply(new_component_ids2,
                              get_concept)
names(new_component_objs2) <- lapply(new_component_objs2,
                                     function(x) slot(x, "concept_name"))

## -----------------------------------------------------------------------------
new_component_objs2

## -----------------------------------------------------------------------------
new_component_permutations <- list()
for (i in seq_along(new_component_objs2)) {
  new_component_permutations[[i]] <-
  new_component_objs2[-i]
  names(new_component_permutations)[i] <- new_component_objs2[[i]]@concept_name
}

## ----results='hide',message=F-------------------------------------------------
new_regimens2 <- list()
for (i in seq_along(new_component_permutations)) {
  
  new_regimens2[[i]] <-
  ho_lookup_regimen(new_component_permutations[[i]])
}
names(new_regimens2) <- names(new_component_permutations)
new_regimens2 <-
  new_regimens2 %>%
  purrr::keep(~ nrow(.) > 0)

## -----------------------------------------------------------------------------
new_regimens2

## -----------------------------------------------------------------------------
new_regimens2 %>% 
  bind_rows(.id = "add_on_component_name") %>%
  select(add_on_component_name, regimen_concept_id, regimen_concept_name) %>%
  distinct()

