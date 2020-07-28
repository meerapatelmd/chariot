#' Create a CONCEPT_PARENT Table
#' @description This function creates a subset of the CONCEPT_ANCESTOR Table where either max_ or min_ levels of separation are 0 or 1. This table is created to visualize the hierarchy of concepts using external R packages such as collapsibleTrees.
#' @import pg13
#' @import dplyr
#' @export




createConceptParentTable <-
        function(schema) {
                conn <- connectAthena()
                pg13::dropTable(conn = conn,
                                schema = schema,
                                tableName = "concept_parent")


                .output <-
                        dplyr::bind_rows(
                                query_athena(
                                pg13::buildQuery(schema = schema,
                                                 tableName = "concept_ancestor",
                                                 whereInField = "min_levels_of_separation",
                                                 whereInVector = c(0,1),
                                                 caseInsensitive = FALSE)),
                                query_athena(
                                        pg13::buildQuery(schema = schema,
                                                         tableName = "concept_ancestor",
                                                         whereInField = "max_levels_of_separation",
                                                         whereInVector = c(0,1),
                                                         caseInsensitive = FALSE))
                        ) %>%
                        dplyr::distinct()

                pg13::writeTable(conn = conn,
                                 schema = schema,
                                 tableName = "concept_parent",
                                 .data = .output %>%
                                                dplyr::rename(parent_concept_id = ancestor_concept_id,
                                                              child_concept_id = descendant_concept_id))


                dcAthena(conn = conn)


        }
