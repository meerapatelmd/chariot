

# make_conceptTree <- 
#     function(concept_id,
#              ancestor_generations,
#              descendant_generations) {
#         
#         require(collapsibleTree)
#         
#         conceptTree <- list()
#         conceptTree[[1]] <- concept_parents(concept_id, generations = ancestor_generations)
#         conceptTree[[2]] <- concept_children(concept_id, generations = descendant_generations)
#         names(conceptTree) <- c("Parent", "Child")
#         
#         # Adding top NA row for parent
#         topmost_parent <-
#         conceptTree$Parent %>%
#             rubix::summarize_grouped_n(parent_concept_id,
#                                        desc = TRUE) %>%
#             dplyr::filter(n == max(n, na.rm = TRUE))
#         
#         if (nrow(topmost_parent) == 1) {
#             
#                 topmost_parent <- 
#                     topmost_parent %>%
#                     dplyr::rename(child_concept_id = parent_concept_id) %>%
#                     dplyr::mutate(parent_concept_id = NA_character_)
#                 
#         } else if (nrow(topmost_parent) > 1) {
#             
#                 topmost_parent <-
#                 topmost_parent %>%
#                     dplyr::rename(child_concept_id = parent_concept_id) %>%
#                     dplyr::mutate(parent_concept_id = "***") %>%
#                     dplyr::mutate_all(as.character) %>%
#                     dplyr::bind_rows(tibble(parent_concept_id = NA_character_,
#                                             child_concept_id = "***")) %>%
#                     dplyr::select(-n)
#                 
#         } else {
#             
#                 stop("Cannot find topmost parent")
#         }
#         
#         conceptTree2 <- 
#             dplyr::bind_rows(topmost_parent,
#                             dplyr::bind_rows(conceptTree))
#         
#         conceptTree3 <- 
#             left_join_concept(conceptTree2,
#                               .col = "parent_concept_id",
#                               include_synonyms = FALSE) %>%
#             chariot::merge_concepts(into = "ParentConcept") %>%
#             dplyr::select(parent_concept_id,
#                           ParentConcept,
#                           child_concept_id)
#         
#         conceptTree4 <- 
#             left_join_concept(conceptTree3,
#                               .col = "child_concept_id",
#                               include_synonyms = FALSE) %>%
#             chariot::merge_concepts(into = "ChildConcept") %>%
#             dplyr::select(-concept_id)
#         
#         
#         
#         
#         conceptTree3 <-
#             conceptTree2 %>%
#                 purrr::map(function(x) left_join_concept(x,
#                                                          .col = "child_concept_id",
#                                                          include_synonyms = FALSE) %>%
#                                dplyr::select(-child_concept_id) %>%
#                                dplyr::rename_at(vars(concept_id:invalid_reason),
#                                                 function(x) paste0("child_", x)))
#         
#         
#         
#         
#         conceptTree_NA_row <- 
#             conceptTree2 %>%
#             group_by(parent_concept_id) %>%
#             mutate(child_count = length(unique(child_concept_id))) %>%
#             ungroup() %>%
#             arrange(desc(child_count)) %>%
#             rubix::filter_first_row() %>%
#             rename_at(vars(starts_with("child_")), 
#                       function(x) stringr::str_remove_all(x, "child_")) %>%
#             rename_at(vars(starts_with("parent_")),
#                       function(x) stringr::str_replace_all(x, "parent_", "child_")) %>%
#             mutate_at(vars(!starts_with("child_")),
#                       function(x) NA_character_) %>%
#             rename_at(vars(!starts_with("child_")),
#                       function(x) paste0("parent_", x))
#         
#         conceptTree2 <- 
#             dplyr::bind_rows(conceptTree_NA_row,
#                              conceptTree2)
#         
#         collapsibleTree::collapsibleTreeNetwork(df = conceptTree2 %>%
#                                                     dplyr::select(parent_concept_id,
#                                                                   child_concept_id) %>%
#                                                     dplyr::distinct())
#         
#         
#     }
