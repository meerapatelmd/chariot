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
             writeSchema,
             vocabulary_id,
             concept_class_id,
             domain_id,
             standard_concept,
             color_col,
             generations,
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
                dplyr::distinct() %>%
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
                dplyr::distinct() %>%
                dplyr::select(-count, -total)


            tryCatch(
            collapsibleTree::collapsibleTreeNetwork(
                    df = updated_levels,
                    tooltipHtml = "tooltip",
                    fill = "color"
            ),
            error = function(e) return(updated_levels))


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





plot_concept_ancestors <-
    function(concept_ids,
             ancestor_generations = 2,
             descendant_generations = 2,
             color_col = concept_class_id,
             conn = NULL,
             writeSchema = "public") {


        output_a <- list()
        output_a[[1]] <-
           queryAthena(sql_statement =
                           SqlRender::render(
                                "
                                SELECT *
                                FROM @vocabSchema.concept_ancestor ca
                                WHERE ca.descendant_concept_id IN (@concept_ids)
                                    AND ca.max_levels_of_separation = 1
                                ",
                                vocabSchema = vocabSchema,
                                concept_ids = concept_ids),
                       conn = conn
                       )


        for (i in 2:ancestor_generations) {
                    new_descendant_concept_ids <- output_a[[i-1]]$ancestor_concept_id
                    output_a[[i]] <-
                        queryAthena(sql_statement =
                                        SqlRender::render(
                                            "
                                SELECT *
                                FROM @vocabSchema.concept_ancestor ca
                                WHERE ca.descendant_concept_id IN (@new_descendant_concept_ids)
                                    AND ca.max_levels_of_separation = 1
                                ",
                                            vocabSchema = vocabSchema,
                                            new_descendant_concept_ids = new_descendant_concept_ids),
                                    conn = conn
                        )

        }

        output_a <- rev(output_a)

        output_d <- list()
        output_d[[1]] <-
            queryAthena(sql_statement =
                            SqlRender::render(
                                "
                                SELECT *
                                FROM @vocabSchema.concept_ancestor ca
                                WHERE ca.ancestor_concept_id IN (@concept_ids)
                                    AND ca.max_levels_of_separation = 1
                                ",
                                vocabSchema = vocabSchema,
                                concept_ids = concept_ids),
                        conn = conn
            )


            for (i in 2:descendant_generations) {
                new_ancestor_concept_ids <- output_a[[i-1]]$descendant_concept_id
                output_d[[i]] <-
                    queryAthena(sql_statement =
                                    SqlRender::render(
                                        "
                                    SELECT *
                                    FROM @vocabSchema.concept_ancestor ca
                                    WHERE ca.ancestor_concept_id IN (@new_ancestor_concept_ids)
                                        AND ca.max_levels_of_separation = 1
                                    ",
                                        vocabSchema = vocabSchema,
                                        new_ancestor_concept_ids = new_ancestor_concept_ids),
                                conn = conn
                    )

            }

        output <-
        c(output_a,
          output_d)

        # level 1
        level_1 <-
            tibble::tibble(descendant_concept_id = unique(output[[1]]$ancestor_concept_id)) %>%
            dplyr::mutate(ancestor_concept_id = 00000) %>%
            dplyr::select(ancestor_concept_id,
                          descendant_concept_id)

        # root
        root <-
            tibble::tibble(ancestor_concept_id = NA,
                           descendant_concept_id = 00000)


        final <-
            dplyr::bind_rows(root,
                             level_1,
                             output) %>%
            dplyr::select(parent_concept_id = ancestor_concept_id,
                          child_concept_id = descendant_concept_id) %>%
            dplyr::distinct() %>%
            tibble::rowid_to_column() %>%
            tidyr::pivot_longer(cols = c(parent_concept_id, child_concept_id),
                                names_to = "relative_type",
                                values_to = "relative_concept_id") %>%
            leftJoinConceptId(column = "relative_concept_id",
                              writeSchema = writeSchema)


        final2 <-
            final %>%
            dplyr::mutate(color := {{color_col}}) %>%
            dplyr::mutate(color = factor(color))

        levels(final2$color) <- colorspace::terrain_hcl(n = length(levels(final2$color)))

        final3 <-
            final2 %>%
            dplyr::mutate_at(vars(!c(rowid, relative_type, relative_concept_id,color)), as.character) %>%
            tidyr::pivot_longer(cols = !c(rowid, relative_type, relative_concept_id,color),
                                names_to = "concept_field",
                                values_to = "concept_field_values",
                                values_drop_na = TRUE) %>%
            dplyr::distinct()


        final4 <-
            dplyr::left_join(final3,
                             final3 %>%
                            tidyr::unite(col = tooltip,
                                         concept_field,
                                         concept_field_values,
                                         sep = ": ",
                                         remove = TRUE,
                                         na.rm = TRUE) %>%
                            dplyr::group_by(rowid, relative_type, relative_concept_id) %>%
                            dplyr::summarize_at(vars(tooltip), ~paste(unique(.), collapse = "<br>")) %>%
                                dplyr::ungroup()) %>%
            dplyr::select(-concept_field, -concept_field_values) %>%
            dplyr::distinct() %>%
            tidyr::pivot_wider(id_col = c(rowid,color),
                               names_from = relative_type,
                               values_from = c(relative_concept_id))


        return(final4)


        return(final)


            resultset2 <-
                dplyr::left_join(resultset,
                            resultset %>%
                                dplyr::mutate_all(as.character) %>%
                                tidyr::pivot_longer(cols = !c(rowid, relationship_id),
                                                    names_to = c("concept_attribute_type", "concept_order"),
                                                    names_pattern = "(^.*)_([1-2]{1}$)",
                                                    values_to = "concept_attribute",
                                                    values_drop_na = TRUE) %>%
                                dplyr::distinct() %>%
                                tidyr::unite(col = tooltip,
                                             concept_attribute_type,
                                             concept_attribute,
                                             sep = ": ",
                                             remove = TRUE,
                                             na.rm = TRUE) %>%
                                dplyr::group_by(rowid, concept_order) %>%
                                dplyr::summarize_at(vars(tooltip), ~paste(unique(.), collapse = "<br>")) %>%
                                tidyr::pivot_wider(names_from = concept_order,
                                                   names_prefix = "concept_tooltip_",
                                                   values_from = tooltip) %>%
                                dplyr::ungroup()
                        ) %>%
                        dplyr::distinct()

            resultset3 <-
                resultset2 %>%
                tidyr::unite(col = concept_1,
                             concept_id_1,
                             concept_name_1,
                             sep = " ",
                             na.rm = TRUE,
                             remove = FALSE) %>%
                tidyr::unite(col = concept_2,
                             concept_id_2,
                             concept_name_2,
                             sep = " ",
                             na.rm = TRUE,
                             remove = FALSE)

            resultset4 <-
                resultset3 %>%
                dplyr::mutate(color = relationship_id) %>%
                dplyr::mutate(color = factor(color))

            levels(resultset4$color) <- colorspace::sequential_hcl(n = length(levels(resultset4$color)))

            resultset5 <-
                resultset4 %>%
                dplyr::select(concept_1,
                              concept_tooltip_1,
                              relationship_id,
                              concept_2,
                              concept_tooltip_2,
                              color)


            root <-
                resultset5 %>%
                dplyr::transmute(
                    parent = NA,
                    child = concept_1,
                    tooltip= concept_tooltip_1,
                    color = "black") %>%
                dplyr::distinct()

            level_1 <-
                resultset5 %>%
                dplyr::transmute(
                    parent = concept_1,
                    child = relationship_id,
                    tooltip= "",
                    color = color) %>%
                dplyr::distinct()


            level_2 <-
                resultset5 %>%
                dplyr::transmute(
                    parent = relationship_id,
                    child = concept_2,
                    tooltip= concept_tooltip_2,
                    color = color) %>%
                dplyr::distinct()



            final_a <-
                dplyr::bind_rows(root,
                                 level_1)



            final_b <-
                dplyr::bind_rows(level_2) %>%
                dplyr::group_by(child) %>%
                dplyr::mutate(total = n()) %>%
                dplyr::mutate(tally = 1:n()) %>%
                dplyr::ungroup() %>%
                dplyr::mutate(append_label = ifelse(total > 1, paste0("(", tally, ")"), NA_character_)) %>%
                tidyr::unite(col = child2,
                             child,
                             append_label,
                             sep = " ",
                             remove = TRUE,
                             na.rm = TRUE) %>%
                dplyr::rename(child = child2) %>%
                dplyr::select(-total, -tally)

            final <-
                dplyr::bind_rows(final_a,
                                 final_b)

            collapsibleTree::collapsibleTreeNetwork(df = final,
                                                    tooltipHtml = "tooltip",
                                                    fill = "color")

    }



plot_concept_relationships <-
    function(concept_ids,
             color_col,
             conn = NULL) {

        resultset <-
            queryAthena(sql_statement =
                            SqlRender::render(
                                "
                                SELECT
                                    c1.concept_id AS concept_id_1,
                                    c1.concept_name AS concept_name_1,
                                    c1.domain_id AS domain_id_1,
                                    c1.concept_class_id AS concept_class_id_1,
                                    c1.standard_concept AS standard_concept_1,
                                    cr.relationship_id,
                                    c2.concept_id AS concept_id_2,
                                    c2.concept_name AS concept_name_2,
                                    c2.domain_id AS domain_id_2,
                                    c2.concept_class_id AS concept_class_id_2,
                                    c2.standard_concept AS standard_concept_2
                                FROM @vocabSchema.concept_relationship cr
                                LEFT JOIN @vocabSchema.concept c1
                                ON c1.concept_id = cr.concept_id_1
                                LEFT JOIN @vocabSchema.concept c2
                                ON c2.concept_id = cr.concept_id_2
                                WHERE cr.concept_id_1 = (@concept_ids)
                                    AND cr.invalid_reason IS NULL
                                    AND c1.invalid_reason IS NULL
                                    AND c2.invalid_reason IS NULL
                                ",
                                vocabSchema = vocabSchema,
                                concept_ids = concept_ids),
                        conn = conn
            ) %>%
            tibble::rowid_to_column() %>%
            dplyr::mutate(rowid = as.character(rowid))

        resultset2 <-
            dplyr::left_join(resultset,
                             resultset %>%
                                 dplyr::mutate_all(as.character) %>%
                                 tidyr::pivot_longer(cols = !c(rowid, relationship_id),
                                                     names_to = c("concept_attribute_type", "concept_order"),
                                                     names_pattern = "(^.*)_([1-2]{1}$)",
                                                     values_to = "concept_attribute",
                                                     values_drop_na = TRUE) %>%
                                 dplyr::distinct() %>%
                                 tidyr::unite(col = tooltip,
                                              concept_attribute_type,
                                              concept_attribute,
                                              sep = ": ",
                                              remove = TRUE,
                                              na.rm = TRUE) %>%
                                 dplyr::group_by(rowid, concept_order) %>%
                                 dplyr::summarize_at(vars(tooltip), ~paste(unique(.), collapse = "<br>")) %>%
                                 tidyr::pivot_wider(names_from = concept_order,
                                                    names_prefix = "concept_tooltip_",
                                                    values_from = tooltip) %>%
                                 dplyr::ungroup()
            ) %>%
            dplyr::distinct()

        resultset3 <-
            resultset2 %>%
            tidyr::unite(col = concept_1,
                         concept_id_1,
                         concept_name_1,
                         sep = " ",
                         na.rm = TRUE,
                         remove = FALSE) %>%
            tidyr::unite(col = concept_2,
                         concept_id_2,
                         concept_name_2,
                         sep = " ",
                         na.rm = TRUE,
                         remove = FALSE)

        resultset4 <-
            resultset3 %>%
            dplyr::mutate(color = relationship_id) %>%
            dplyr::mutate(color = factor(color))

        levels(resultset4$color) <- colorspace::sequential_hcl(n = length(levels(resultset4$color)))

        resultset5 <-
            resultset4 %>%
            dplyr::select(concept_1,
                          concept_tooltip_1,
                          relationship_id,
                          concept_2,
                          concept_tooltip_2,
                          color)


        root <-
            resultset5 %>%
            dplyr::transmute(
                parent = NA,
                child = concept_1,
                tooltip= concept_tooltip_1,
                color = "black") %>%
            dplyr::distinct()

        level_1 <-
            resultset5 %>%
            dplyr::transmute(
                parent = concept_1,
                child = relationship_id,
                tooltip= "",
                color = color) %>%
            dplyr::distinct()


        level_2 <-
            resultset5 %>%
            dplyr::transmute(
                parent = relationship_id,
                child = concept_2,
                tooltip= concept_tooltip_2,
                color = color) %>%
            dplyr::distinct()



        final_a <-
            dplyr::bind_rows(root,
                             level_1)



        final_b <-
            dplyr::bind_rows(level_2) %>%
            dplyr::group_by(child) %>%
            dplyr::mutate(total = n()) %>%
            dplyr::mutate(tally = 1:n()) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(append_label = ifelse(total > 1, paste0("(", tally, ")"), NA_character_)) %>%
            tidyr::unite(col = child2,
                         child,
                         append_label,
                         sep = " ",
                         remove = TRUE,
                         na.rm = TRUE) %>%
            dplyr::rename(child = child2) %>%
            dplyr::select(-total, -tally)

        final <-
            dplyr::bind_rows(final_a,
                             final_b)

        collapsibleTree::collapsibleTreeNetwork(df = final,
                                                tooltipHtml = "tooltip",
                                                fill = "color")

    }
