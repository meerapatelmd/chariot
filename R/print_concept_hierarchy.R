





print_concept_hierarchy <-
        function(concept_obj) {

                concept_id <- concept_obj@concept_id

                target_concept <- get_strip(concept_id)
                target_concept <- secretary::enbold(sprintf("*%s", target_concept))

                data <- tibble::tibble(concept_hierarchy_id = concept_id)

                ancestors <-
                        join_for_ancestors(data = data,
                                           descendant_id_column = "concept_hierarchy_id") %>%
                        merge_strip(into = "ancestor",
                                    prefix = "ancestor_") %>%
                        rubix::split_by(col = min_levels_of_separation) %>%
                        rev() %>%
                        purrr::map(function(x) x %>%
                                           select(ancestor) %>%
                                           unlist() %>%
                                           unname())

                # Removed level 0 because can have >900 concepts at this level
                ancestors$`0` <- NULL

                ancestors[[length(ancestors)+1]] <- target_concept


                descendants <-
                        join_for_descendants(
                                data = data,
                                ancestor_id_column = "concept_hierarchy_id"
                        ) %>%
                        merge_strip(into = "descendant",
                                    prefix = "descendant_") %>%
                        dplyr::arrange(min_levels_of_separation) %>%
                        rubix::split_by(col = min_levels_of_separation) %>%
                        purrr::map(function(x) x %>%
                                           select(descendant) %>%
                                           unlist() %>%
                                           unname())

                # Removed level 0 because can have >900 concepts at this level
                descendants$`0` <- NULL

                for (i in seq_along(ancestors)) {

                        if (length(ancestors[[i]]) <= 10) {
                                cat(sprintf("%s%s\n", paste(rep("\t", i), collapse = ""), ancestors[[i]]))

                        } else {
                                cat(sprintf("%s%s\n", paste(rep("\t", i), collapse = ""), c(ancestors[[i]][1:10], "...")))
                        }
                }

                for (j in seq_along(descendants)) {

                        if (length(descendants[[j]]) <= 10) {
                        cat(sprintf("%s%s\n", paste(rep("\t", i+j), collapse = ""), descendants[[j]]))
                        } else {
                                cat(sprintf("%s%s\n", paste(rep("\t", i+j), collapse = ""), c(descendants[[j]][1:10], "...")))
                        }
                }



        }
