#' Query concept children
#' @import dplyr
#' @export


concept_children <-
    function(parent_id,
             generations = 1,
             override_cache = FALSE) {

            .Deprecated()


                baseline_child <-
                    query_athena(paste0("SELECT * FROM concept_parent WHERE parent_concept_id = '", parent_id, "';"))

                if (nrow(baseline_child) == 0) {

                        warning('concept "', parent_id, '" has no children')

                        return(NULL)

                } else {

                output <- list()
                output[[1]] <- baseline_child

                for (i in 1:generations) {

                    if (i != 1) {

                            prior <- output[[(i-1)]]

                            #secretary::press_enter()
                            output[[i]] <- left_join_for_children(prior %>%
                                                           dplyr::select(prior_child_concept_id = child_concept_id)) %>%
                                dplyr::filter(!is.na(prior_child_concept_id))



                                        # prior %>%
                                        # dplyr::inner_join(concept_parent_table,
                                        #                   by = c("child_concept_id" = "parent_concept_id"),
                                        #                   suffix = c(".prior", ".new")) %>%
                                        # dplyr::select(parent_concept_id = child_concept_id,
                                        #               child_concept_id = child_concept_id.new)

                    }

                }

                if (length(output) != generations) {

                    message('Maximum possible generations less than "generations" param:', length(output))
                }

                output %>%
                    dplyr::bind_rows() %>%
                    dplyr::select(parent_concept_id, child_concept_id)
                }

    }


