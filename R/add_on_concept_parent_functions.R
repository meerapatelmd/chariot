#' Create a CONCEPT_PARENT Table
#' @description This function creates a subset of the CONCEPT_ANCESTOR Table where either max_ or min_ levels of separation are 0 or 1 combined with the CONCEPT_RELATIONSHIP Table where there is a "Subsumes" or "Is a" relationship between concept_id_1 and concept_id_2. This table is created to visualize the hierarchy of concepts using external R packages such as collapsibleTrees.
#' @import pg13
#' @import dplyr
#' @export

createConceptParentTable <-
        function(schema,
                 override_cache = FALSE,
                 render_sql = TRUE,
                 conn = NULL) {

                if (is.null(conn)) {
                        conn <- connectAthena()

                        pg13::dropTable(conn = conn,
                                        schema = schema,
                                        tableName = "concept_parent")


                        # .output_a <-
                        #         dplyr::bind_rows(
                        #                 queryAthena(
                        #                 pg13::buildQuery(schema = schema,
                        #                                  tableName = "concept_ancestor",
                        #                                  whereInField = "min_levels_of_separation",
                        #                                  whereInVector = c(0,1),
                        #                                  caseInsensitive = FALSE),
                        #                         override_cache = override_cache,
                        #                         render_sql = render_sql,
                        #                         conn = conn),
                        #                 queryAthena(
                        #                         pg13::buildQuery(schema = schema,
                        #                                          tableName = "concept_ancestor",
                        #                                          whereInField = "max_levels_of_separation",
                        #                                          whereInVector = c(0,1),
                        #                                          caseInsensitive = FALSE),
                        #                         override_cache = override_cache,
                        #                         render_sql = render_sql,
                        #                         conn = conn)
                        #         ) %>%
                        #         dplyr::distinct() %>%
                        #         dplyr::select(parent_concept_id = ancestor_concept_id,
                        #                       child_concept_id = descendant_concept_id)


                        .output_b <-
                                queryAthena(pg13::buildQuery(schema = schema,
                                                             tableName = "concept_relationship",
                                                             whereInField = "relationship_id",
                                                             whereInVector = "Subsumes"),
                                            override_cache = override_cache,
                                            render_sql = render_sql,
                                            conn = conn) %>%
                                dplyr::filter(is.na(invalid_reason)) %>%
                                dplyr::select(parent_concept_id = concept_id_1,
                                              child_concept_id = concept_id_2)


                        .output_c <-
                                queryAthena(pg13::buildQuery(schema = schema,
                                                             tableName = "concept_relationship",
                                                             whereInField = "relationship_id",
                                                             whereInVector = "Is a"),
                                            override_cache = override_cache,
                                            render_sql = render_sql,
                                            conn = conn) %>%
                                dplyr::filter(is.na(invalid_reason)) %>%
                                dplyr::select(parent_concept_id = concept_id_2,
                                              child_concept_id = concept_id_1)

                        pg13::writeTable(conn = conn,
                                         schema = schema,
                                         tableName = "concept_parent",
                                         .data = dplyr::bind_rows(.output_b,
                                                                  .output_c) %>%
                                                 dplyr::distinct() %>%
                                                 dplyr::filter(parent_concept_id != child_concept_id))



                        dcAthena(conn = conn)


                } else {

                        pg13::dropTable(conn = conn,
                                        schema = schema,
                                        tableName = "concept_parent")


                        # .output_a <-
                        #         dplyr::bind_rows(
                        #                 queryAthena(
                        #                 pg13::buildQuery(schema = schema,
                        #                                  tableName = "concept_ancestor",
                        #                                  whereInField = "min_levels_of_separation",
                        #                                  whereInVector = c(0,1),
                        #                                  caseInsensitive = FALSE),
                        #                         override_cache = override_cache,
                        #                         render_sql = render_sql,
                        #                         conn = conn),
                        #                 queryAthena(
                        #                         pg13::buildQuery(schema = schema,
                        #                                          tableName = "concept_ancestor",
                        #                                          whereInField = "max_levels_of_separation",
                        #                                          whereInVector = c(0,1),
                        #                                          caseInsensitive = FALSE),
                        #                         override_cache = override_cache,
                        #                         render_sql = render_sql,
                        #                         conn = conn)
                        #         ) %>%
                        #         dplyr::distinct() %>%
                        #         dplyr::select(parent_concept_id = ancestor_concept_id,
                        #                       child_concept_id = descendant_concept_id)


                        .output_b <-
                                queryAthena(pg13::buildQuery(schema = schema,
                                                             tableName = "concept_relationship",
                                                             whereInField = "relationship_id",
                                                             whereInVector = "Subsumes"),
                                            override_cache = override_cache,
                                            render_sql = render_sql,
                                            conn = conn) %>%
                                dplyr::filter(is.na(invalid_reason)) %>%
                                dplyr::select(parent_concept_id = concept_id_1,
                                              child_concept_id = concept_id_2)


                        .output_c <-
                                queryAthena(pg13::buildQuery(schema = schema,
                                                             tableName = "concept_relationship",
                                                             whereInField = "relationship_id",
                                                             whereInVector = "Is a"),
                                            override_cache = override_cache,
                                            render_sql = render_sql,
                                            conn = conn) %>%
                                dplyr::filter(is.na(invalid_reason)) %>%
                                dplyr::select(parent_concept_id = concept_id_2,
                                              child_concept_id = concept_id_1)

                        pg13::writeTable(conn = conn,
                                         schema = schema,
                                         tableName = "concept_parent",
                                         .data = dplyr::bind_rows(.output_b,
                                                                  .output_c) %>%
                                                 dplyr::distinct() %>%
                                                 dplyr::filter(parent_concept_id != child_concept_id))



                }


        }



#' Query concept parents
#' @import dplyr
#' @export

concept_parents <-
    function(child_id,
             generations = 1,
             override_cache = FALSE) {

                baseline_parent <-
                    query_athena(paste0("SELECT * FROM concept_parent WHERE child_concept_id = '", child_id, "';"))

                output <- list()
                output[[1]] <- baseline_parent

                for (i in 1:generations) {

                    if (i != 1) {

                            prior <- output[[(i-1)]]

                            #secretary::press_enter()
                            output[[i]] <-
                                left_join_for_parents(prior %>%
                                                          dplyr::select(prior_parent_concept_id = parent_concept_id)) %>%
                                dplyr::filter(!is.na(prior_parent_concept_id))
                                        # concept_parent_table %>%
                                        # dplyr::inner_join(prior,
                                        #                   by = c("child_concept_id" = "parent_concept_id"),
                                        #                   suffix = c(".prior", ".new")) %>%
                                        # dplyr::select(parent_concept_id,
                                        #               child_concept_id)

                    }

                }

                output2 <-
                     output %>%
                    purrr::keep(~nrow(.)>0)

                if (length(output2) != generations) {

                        warning('Maximum possible generations less than "generations" param: ', length(output2))
                }

                output2 %>%
                    dplyr::bind_rows() %>%
                    dplyr::mutate(child_concept_id = coalesce(prior_parent_concept_id, child_concept_id)) %>%
                    dplyr::select(parent_concept_id,
                                  child_concept_id)

    }


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

