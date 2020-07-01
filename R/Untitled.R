





library(collapsibleTree)

conceptTree <- list()
conceptTree[[1]] <- concept_parents("19080128", generations = 2)
conceptTree[[2]] <- concept_children("19080128", generations = 2)

conceptTree2 <-
    conceptTree %>%
    purrr::map(function(x) chariot::left_join_concept_id(x,
                                                         dataframe_column = "parent_concept_id",
                                                         include_synonyms = FALSE) %>%
                            dplyr::select(-parent_concept_id) %>%
                            dplyr::rename_at(vars(concept_id:last_col()),
                                             function(x) paste0("parent_", x))
                   ) %>%
    purrr::map(function(x) chariot::left_join_concept_id(x,
                                                         dataframe_column = "child_concept_id",
                                                         include_synonyms = FALSE) %>%
                   dplyr::select(-child_concept_id) %>%
                   dplyr::rename_at(vars(concept_id:last_col()),
                                    function(x) paste0("child_", x))
    ) %>%
    dplyr::bind_rows() %>%
    mutate_all(stringr::str_replace_all, "NA", NA_character_) 


conceptTree_NA_row <- 
    conceptTree2 %>%
    group_by(parent_concept_id) %>%
    mutate(child_count = length(unique(child_concept_id))) %>%
    ungroup() %>%
    arrange(desc(child_count)) %>%
    rubix::filter_first_row() %>%
    rename_at(vars(starts_with("child_")), 
              function(x) stringr::str_remove_all(x, "child_")) %>%
    rename_at(vars(starts_with("parent_")),
              function(x) stringr::str_replace_all(x, "parent_", "child_")) %>%
    mutate_at(vars(!starts_with("child_")),
              function(x) NA_character_) %>%
    rename_at(vars(!starts_with("child_")),
              function(x) paste0("parent_", x))

conceptTree2 <- 
    dplyr::bind_rows(conceptTree_NA_row,
                     conceptTree2)

collapsibleTreeNetwork(df = conceptTree2 %>%
                           dplyr::select(parent_concept_id,
                                         child_concept_id) %>%
                           dplyr::distinct())
