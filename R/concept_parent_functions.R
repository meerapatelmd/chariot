#' @title
#' Write a CONCEPT_PARENT Table
#' @description
#' This function creates a subset of the CONCEPT_RELATIONSHIP Table where all forward "Subsumes" relationships are subset. The Concept Ancestor Table is related to the Concept Relationship Table by whether or not the relationship_id in the Concept Relationship Table defines ancestry according to the Relationship Table.
#' @import pg13
#' @import dplyr
#' @export

writeConceptParentTable <-
    function(vocabSchema,
             render_sql = TRUE,
             conn = NULL) {

            if (is.null(conn)) {
                write_conn <- connectAthena()
            } else {
                write_conn <- conn
            }

            sendAthena(conn = write_conn,
                       render_sql = render_sql,
                       sql_statement =
                                SqlRender::render(
                                "
                                DROP TABLE IF EXISTS @vocabSchema.concept_parent;
                                CREATE TABLE @vocabSchema.concept_parent (
                                                parent_concept_id INTEGER,
                                                child_concept_id INTEGER
                                );

                                WITH new_concept_parent AS (
                                        SELECT DISTINCT
                                                cr.concept_id_1 AS parent_concept_id,
                                                cr.concept_id_2 AS child_concept_id
                                        FROM @vocabSchema.concept_relationship cr
                                        LEFT JOIN @vocabSchema.concept c1
                                        ON cr.concept_id_1 = c1.concept_id
                                        LEFT JOIN @vocabSchema.concept c2
                                        ON cr.concept_id_2 = c2.concept_id
                                        WHERE cr.relationship_id = 'Subsumes'
                                        AND cr.concept_id_1 <> cr.concept_id_2
                                        AND cr.invalid_reason IS NULL
                                        AND c1.invalid_reason IS NULL
                                        AND c2.invalid_reason IS NULL
                                ),
                                inverse AS (
                                        SELECT DISTINCT
                                                cr.concept_id_2 AS parent_concept_id,
                                                cr.concept_id_1 AS child_concept_id
                                        FROM @vocabSchema.concept_relationship cr
                                        LEFT JOIN @vocabSchema.concept c1
                                        ON cr.concept_id_1 = c1.concept_id
                                        LEFT JOIN @vocabSchema.concept c2
                                        ON cr.concept_id_2 = c2.concept_id
                                        WHERE cr.relationship_id = 'Is a'
                                        AND cr.concept_id_1 <> cr.concept_id_2
                                        AND cr.invalid_reason IS NULL
                                        AND c1.invalid_reason IS NULL
                                        AND c2.invalid_reason IS NULL
                                ),
                                combined AS (
                                    SELECT *
                                    FROM new_concept_parent
                                    UNION
                                    SELECT *
                                    FROM inverse
                                )

                                INSERT INTO @vocabSchema.concept_parent SELECT DISTINCT * FROM combined;
                                ",
                                vocabSchema = vocabSchema))




        if (is.null(conn)) {
            dcAthena(conn = write_conn)
        }


    }


#' @title
#' Write a CONCEPT_PARENT Table
#' @description
#' This function creates a subset of the CONCEPT_RELATIONSHIP Table where all forward "Subsumes" relationships are subset. The Concept Ancestor Table is related to the Concept Relationship Table by whether or not the relationship_id in the Concept Relationship Table defines ancestry according to the Relationship Table.
#' @import pg13
#' @import dplyr
#' @export

writeConceptRelationshipMapTable <-
    function(vocabSchema,
             render_sql = TRUE,
             conn = NULL) {

        if (is.null(conn)) {
            write_conn <- connectAthena()
        } else {
            write_conn <- conn
        }

        sendAthena(conn = write_conn,
                   render_sql = render_sql,
                   sql_statement =
                       SqlRender::render(
                           "
                                DROP TABLE IF EXISTS @vocabSchema.concept_relationship_map;
                                CREATE TABLE @vocabSchema.concept_relationship_map (
                                                domain_id_a VARCHAR(20),
                                                vocabulary_id_a VARCHAR(20),
                                                concept_class_id_a VARCHAR(20),
                                                standard_concept_a VARCHAR(1),
                                                relationship_id VARCHAR(20),
                                                 domain_id_b VARCHAR(20),
                                                vocabulary_id_b VARCHAR(20),
                                                concept_class_id_b VARCHAR(20),
                                                standard_concept_b VARCHAR(1)
                                );

                                WITH concepts_ab AS (
                                        SELECT DISTINCT
                                                c1.domain_id AS domain_id_a,
                                                c1.vocabulary_id AS vocabulary_id_a,
                                                c1.concept_class_id AS concept_class_id_a,
                                                c1.standard_concept AS standard_concept_a,
                                                cr.relationship_id,
                                                c2.domain_id AS domain_id_b,
                                                c2.vocabulary_id AS vocabulary_id_b,
                                                c2.concept_class_id AS concept_class_id_b,
                                                c2.standard_concept AS standard_concept_b
                                        FROM @vocabSchema.concept_relationship cr
                                        LEFT JOIN @vocabSchema.concept c1
                                        ON cr.concept_id_1 = c1.concept_id
                                        LEFT JOIN @vocabSchema.concept c2
                                        ON cr.concept_id_2 = c2.concept_id
                                        WHERE
                                            cr.concept_id_1 <> cr.concept_id_2
                                            AND cr.invalid_reason IS NULL
                                            AND c1.invalid_reason IS NULL
                                            AND c2.invalid_reason IS NULL
                                )

                                INSERT INTO @vocabSchema.concept_relationship_map SELECT DISTINCT * FROM concepts_ab;
                                ",
                           vocabSchema = vocabSchema))




        if (is.null(conn)) {
            dcAthena(conn = write_conn)
        }


    }







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

            .Deprecated()

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


#' @title
#' Return all Hierarchical Relationship Ids

return_relationship_ids <-
    function(vocabSchema,
             conn = NULL) {

                queryAthena(SqlRender::render("SELECT * FROM @vocabSchema.relationship;", vocabSchema = vocabSchema),
                            conn = conn)
    }




#' @details invalid_reason is not an argument because it is already filtered out when making the concept_parent table


plot_is_a <-
    function(vocabSchema,
             vocabulary_id,
             concept_class_id,
             domain_id,
             standard_concept,
             color_col,
             generations,
             writeSchema,
             conn = NULL,
             render_sql = TRUE,
             verbose = FALSE,
             sleepTime = 1) {


            concept_filters <- generate_concept_filters(vocabSchema = vocabSchema,
                                                        vocabulary_id = vocabulary_id,
                                                        domain_id = domain_id,
                                                        concept_class_id = concept_class_id,
                                                        standard_concept = standard_concept)

            if (is.null(concept_filters)) {
                stop('`vocabulary_id`, `concept_class_id`, `domain_id`, and/or `standard_concept` required')
            }

            output <-
            queryAthena(
                    SqlRender::render(
                                        "
                                        WITH target_cids AS (
                                                SELECT DISTINCT
                                                            concept_id
                                                FROM @vocabSchema.concept
                                                WHERE @concept_filters
                                        )

                                        SELECT
                                            cp.parent_concept_id,
                                            cp.child_concept_id
                                        FROM @vocabSchema.concept_parent cp
                                        INNER JOIN target_cids t
                                        ON t.concept_id = cp.parent_concept_id
                                        UNION
                                        SELECT
                                             cp.parent_concept_id,
                                             cp.child_concept_id
                                        FROM @vocabSchema.concept_parent cp
                                        INNER JOIN target_cids t
                                        ON t.concept_id = cp.child_concept_id
                                        ;",
                                        vocabSchema = vocabSchema,
                                        concept_filters = concept_filters
                    ),
                    conn = conn,
                    skip_cache = TRUE,
                    render_sql = render_sql,
                    verbose = verbose,
                    sleepTime = sleepTime)

            # Creating the top bud from the topmost concepts
            output_b <-
            output %>% dplyr::full_join(output, by = c("parent_concept_id" = "child_concept_id")) %>% dplyr::rename(grandparent_concept_id = parent_concept_id.y) %>% dplyr::filter(is.na(grandparent_concept_id)) %>%
                dplyr::select(parent_concept_id = grandparent_concept_id,
                              child_concept_id = parent_concept_id) %>%
                dplyr::distinct() %>%
                dplyr::mutate(new_parent_concept_id = 0) %>%
                dplyr::mutate(parent_concept_id = dplyr::coalesce(parent_concept_id, new_parent_concept_id)) %>%
                dplyr::select(-new_parent_concept_id)

            output_c <-
                tibble::tibble(parent_concept_id = NA,
                           child_concept_id = 0)


            output2 <-
            dplyr::bind_rows(output_c,
                             output_b,
                             output) %>%
                dplyr::mutate(child_concept_id = as.integer(child_concept_id))


            output3b <-
            leftJoinConceptId(output2,
                              column = "child_concept_id",
                              writeSchema = writeSchema,
                              conn = conn
                              )

            output4 <-
                output2 %>%
                dplyr::left_join(output3b, by = c("parent_concept_id", "child_concept_id")) %>%
                dplyr::distinct()

            output5 <-
                output4 %>%
                dplyr::mutate(concept_name = ifelse(concept_id == 0, "Bud", concept_name),
                              domain_id = ifelse(concept_id == 0, NA_character_, domain_id),
                              concept_class_id = ifelse(concept_id == 0, NA_character_, concept_class_id))

            tooltip <-
            output5 %>%
                # Converting all columns to be pivoted to character otherwise cannot be combined
                dplyr::mutate_at(vars(!c(parent_concept_id, child_concept_id)), as.character) %>%
                tidyr::pivot_longer(cols = !c(parent_concept_id, child_concept_id),
                                    names_to = "tooltip",
                                    values_to = "tooltip_value",
                                    values_drop_na = TRUE) %>%
                tidyr::unite(col = tooltip,
                             tooltip,
                             tooltip_value,
                             sep = ": ",
                             remove = TRUE,
                             na.rm = TRUE) %>%
                dplyr::group_by(child_concept_id) %>%
                dplyr::summarize_at(vars(tooltip), ~paste(., collapse = "<br>")) %>%
                dplyr::ungroup()

            output6 <-
            output5 %>%
                dplyr::left_join(tooltip) %>%
                dplyr::distinct() %>%
                dplyr::mutate_all(as.character)


            output7 <-
            output6 %>%
                tidyr::unite(col = child,
                             concept_id,
                             concept_name,
                             sep = " ",
                             remove = FALSE,
                             na.rm = TRUE)

            output7b <-
                output7 %>%
                dplyr::mutate(color := {{color_col}}) %>%
                dplyr::mutate(color = factor(color))

            levels(output7b$color) <- colorspace::rainbow_hcl(n = length(levels(output7b$color)))



            output8 <-
            output7b %>%
                dplyr::left_join(output7 %>%
                                     dplyr::select(parent_concept_id = child_concept_id,
                                                   parent = child) %>%
                                     dplyr::distinct(),
                                 by = "parent_concept_id") %>%
                dplyr::distinct()


            output8 <-
                output8 %>%
                dplyr::select(parent,child, parent_concept_id, child_concept_id, tooltip, color) %>%
                dplyr::mutate(parent_concept_id = as.integer(parent_concept_id),
                              child_concept_id = as.integer(child_concept_id))


            levels <- list()
            root <-
                output8 %>%
                dplyr::filter(is.na(parent_concept_id))
            levels[[1]] <- root
            names(levels)[1] <- "root"

            level_1 <-
                output8 %>%
                dplyr::filter(parent_concept_id == 0)
            levels[[2]] <- level_1
            names(levels)[2] <- "level_1"

            proceed <- TRUE
            for (i in 3:20) {

                if (proceed) {

                        x <-
                            levels[[i-1]] %>%
                            dplyr::select(parent = child,
                                          parent_concept_id = child_concept_id) %>%
                            dplyr::inner_join(output8, by = c("parent", "parent_concept_id")) %>%
                            dplyr::select(-child) %>%
                            leftJoinConceptId(column = "child_concept_id", writeSchema = writeSchema) %>%
                            tidyr::unite(col = child,
                                         child_concept_id,
                                         concept_name,
                                         sep = " ",
                                         remove = FALSE,
                                         na.rm = TRUE) %>%
                            dplyr::select(parent_concept_id,
                                          child_concept_id,
                                          parent,
                                          child,
                                          tooltip,
                                          color) %>%
                            dplyr::distinct()

                        if (nrow(x) == 0) {

                                proceed <- FALSE

                        } else {

                                x2 <-
                                    x %>%
                                    dplyr::arrange(child) %>%
                                    dplyr::group_by(child) %>%
                                    dplyr::mutate(count = 1:n()) %>%
                                    dplyr::mutate(total = n()) %>%
                                    dplyr::ungroup() %>%
                                    dplyr::mutate(label_append = ifelse(total > 1,
                                                                        paste0("(", count, ")"),
                                                                        NA)) %>%
                                    tidyr::unite(col = child2,
                                                 child,
                                                 label_append,
                                                 sep = " ",
                                                 remove = TRUE,
                                                 na.rm = TRUE) %>%
                                    dplyr::distinct() %>%
                                    dplyr::rename(child = child2) %>%
                                    dplyr::arrange(parent_concept_id, child_concept_id)

                                levels[[i]] <- x2
                                names(levels)[i] <- paste0("level_", i-1)


                        }

                }
            }

            # Add 1 because root has a slot in the list
            starting_index <- grep(pattern = min(generations), x = names(levels))

            if (length(starting_index)==0) {

                    starting_index <- 3

            }

            ending_index <- grep(pattern = max(generations), x = names(levels))

            if (length(ending_index)==0) {

                ending_index <- length(levels)

            }

            updated_levels <-
                list(purrr::pluck(levels, "root"),
                     purrr::pluck(levels, "level_1"),
                     purrr::pluck(levels, starting_index),
                     purrr::pluck(levels, ending_index))

            updated_levels <-
            updated_levels %>%
                dplyr::bind_rows() %>%
                dplyr::distinct()


            collapsibleTree::collapsibleTreeNetwork(
                    updated_levels,
                    tooltipHtml = "tooltip",
                    fill = "color"
            )


            # level_1 <-
            #     output6 %>%
            #     dplyr::filter(parent_concept_id %in% level_0$child_concept_id)
            #
            # level_2 <-
            #     output6 %>%
            #     dplyr::filter(parent_concept_id %in% level_1$child_concept_id)
            #
            # level_3 <-
            #     output6 %>%
            #     dplyr::filter(parent_concept_id %in% level_2$child_concept_id)
            #
            # level_4 <-
            #     output6 %>%
            #     dplyr::filter(parent_concept_id %in% level_3$child_concept_id)
            #
            # level_5 <-
            #     output6 %>%
            #     dplyr::filter(parent_concept_id %in% level_4$child_concept_id)
            #
            # level_6 <-
            #     output6 %>%
            #     dplyr::filter(parent_concept_id %in% level_5$child_concept_id)


    }





plot_relationships <-
    function() {

            queryAthena("SELECT *
                        FROM public.concept ")

    }




