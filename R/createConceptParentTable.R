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
