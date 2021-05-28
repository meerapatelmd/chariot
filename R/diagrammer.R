




graph_vocabulary <-
        function(vocabulary_id) {

        edges <-
        queryAthena(
                SqlRender::render(
                "
                  WITH vocab AS (
                     SELECT *
                    FROM omop_vocabulary.concept c
                    WHERE
                      c.invalid_reason IS NULL
                      AND c.vocabulary_id = '@vocabulary_id'
                  )

                  SELECT DISTINCT
                    v1.concept_class_id AS concept_class_id_1,
                    v1.standard_concept AS standard_concept_1,
                    cr.relationship_id,
                    v2.concept_class_id AS concept_class_id_2,
                    v2.standard_concept AS standard_concept_2
                  FROM omop_vocabulary.concept_relationship cr
                  INNER JOIN vocab v1
                  ON v1.concept_id = cr.concept_id_1
                  INNER JOIN vocab v2
                  ON v2.concept_id = cr.concept_id_2
                  WHERE cr.invalid_reason IS NULL
                  ",
                vocabulary_id = vocabulary_id)) %>%
                        # True NA values do not split out in list so needs to be replaced with N
                        mutate_at(vars(standard_concept_1,
                                       standard_concept_2), ~ifelse(is.na(.), "N", .))


        if (nrow(edges) == 0) {

                stop(sprintf('"%s" does not have valid relationships.', vocabulary_id))
        }

        nodes <-
                bind_rows(edges %>%
                                  select(ends_with("1")) %>%
                                  rename_all(str_remove_all, "_1$"),
                          edges %>%
                                  select(ends_with("2")) %>%
                                  rename_all(str_remove_all,"_2$")) %>%
                distinct()


        ndf <-
                create_node_df(n = nrow(nodes),
                               type = nodes$standard_concept,
                               label = nodes$concept_class_id,
                               style = "filled",
                               fixedsize = TRUE) %>%
                mutate(fillcolor =
                               case_when(type == "C" ~ "blue",
                                         type == "S" ~ "red",
                                         type == "N" ~ "gray"),
                       shape =
                               case_when(type == "C" ~ "box",
                                         TRUE ~ "circle")
                )


        edf <-
                edges %>%
                left_join(ndf,
                          by = c("standard_concept_1" = "type",
                                 "concept_class_id_1" = "label")) %>%
                rename(from = id) %>%
                left_join(ndf,
                          by = c("standard_concept_2" = "type",
                                 "concept_class_id_2" = "label")) %>%
                rename(to = id) %>%
                select(from,
                       to,
                       rel = relationship_id)

        edf <-
                create_edge_df(from = edf$from,
                               to   = edf$to,
                               rel  = edf$rel,
                               label =edf$rel,
                               len   = 5,
                               fontsize = 14)


        graph <-
                DiagrammeR::create_graph(nodes_df = ndf,
                                         edges_df = edf)

        render_graph(graph)



        }



graph_concept_class <-
        function(vocabulary_id,
                 concept_class_id) {

                edges <-
                        queryAthena(
                                SqlRender::render(
                                        "
                  WITH vocab AS (
                     SELECT *
                    FROM omop_vocabulary.concept c
                    WHERE
                      c.invalid_reason IS NULL
                      AND c.vocabulary_id = '@vocabulary_id'
                  )

                  SELECT DISTINCT
                    v1.concept_class_id AS concept_class_id_1,
                    v1.standard_concept AS standard_concept_1,
                    cr.relationship_id,
                    v2.concept_class_id AS concept_class_id_2,
                    v2.standard_concept AS standard_concept_2
                  FROM omop_vocabulary.concept_relationship cr
                  INNER JOIN vocab v1
                  ON v1.concept_id = cr.concept_id_1
                  INNER JOIN vocab v2
                  ON v2.concept_id = cr.concept_id_2
                  WHERE
                   cr.invalid_reason IS NULL
                   AND v1.concept_class_id = '@concept_class_id'
                  ",
                        vocabulary_id = vocabulary_id,
                        concept_class_id = concept_class_id)) %>%
                        # True NA values do not split out in list so needs to be replaced with N
                        mutate_at(vars(standard_concept_1,
                                       standard_concept_2), ~ifelse(is.na(.), "N", .))


                if (nrow(edges) == 0) {

                        stop(sprintf('"%s" and "%s" do not have valid relationships.', vocabulary_id, concept_class_id))
                }

                nodes <-
                        bind_rows(edges %>%
                                          select(ends_with("1")) %>%
                                          rename_all(str_remove_all, "_1$"),
                                  edges %>%
                                          select(ends_with("2")) %>%
                                          rename_all(str_remove_all,"_2$")) %>%
                        distinct()


                ndf <-
                        create_node_df(n = nrow(nodes),
                                       type = nodes$standard_concept,
                                       label = nodes$concept_class_id,
                                       style = "filled",
                                       fixedsize = TRUE) %>%
                        mutate(fillcolor =
                                       case_when(type == "C" ~ "blue",
                                                 type == "S" ~ "red",
                                                 type == "N" ~ "gray"),
                               shape =
                                       case_when(type == "C" ~ "box",
                                                 TRUE ~ "circle")
                        )


                edf <-
                        edges %>%
                        left_join(ndf,
                                  by = c("standard_concept_1" = "type",
                                         "concept_class_id_1" = "label")) %>%
                        rename(from = id) %>%
                        left_join(ndf,
                                  by = c("standard_concept_2" = "type",
                                         "concept_class_id_2" = "label")) %>%
                        rename(to = id) %>%
                        select(from,
                               to,
                               rel = relationship_id)

                edf <-
                        create_edge_df(from = edf$from,
                                       to   = edf$to,
                                       rel  = edf$rel,
                                       label =edf$rel,
                                       len   = 5,
                                       fontsize = 14)


                graph <-
                        DiagrammeR::create_graph(nodes_df = ndf,
                                                 edges_df = edf)

                render_graph(graph)



        }


graph_concept <-
        function(concept_obj,
                 relationship_type =
                         c("all",
                           "mapping",
                           "taxonomy",
                           "lateral")) {

                relationship_type <-
                        match.arg(
                                arg = relationship_type,
                                choices =  c("all",
                                             "mapping",
                                             "taxonomy",
                                             "lateral"),
                                several.ok = FALSE
                        )

                if (class(concept_obj) == "concept") {
                        concept_id <-
                                concept_obj@concept_id
                } else {

                        concept_id <-
                                concept_obj
                }


                if (relationship_type == "all") {

                        edges <-
                                queryAthena(
                                        SqlRender::render(
                                                "
                          SELECT DISTINCT
                            v1.concept_name AS concept_class_id_1,
                            'R' AS standard_concept_1,
                            cr.relationship_id,
                            v2.concept_class_id AS concept_class_id_2,
                            v2.standard_concept AS standard_concept_2,
                            v2.concept_name AS concept_name_3,
                            'I' AS standard_concept_3
                          FROM omop_vocabulary.concept_relationship cr
                          INNER JOIN omop_vocabulary.concept v1
                          ON v1.concept_id = cr.concept_id_1
                          INNER JOIN omop_vocabulary.concept v2
                          ON v2.concept_id = cr.concept_id_2
                          WHERE
                           cr.invalid_reason IS NULL
                           AND v1.concept_id = @concept_id
                           AND v1.invalid_reason IS NULL
                           AND v2.invalid_reason IS NULL
                          ",
                            concept_id = concept_id)) %>%
                                # True NA values do not split out in list so needs to be replaced with N
                                mutate_at(vars(standard_concept_1,
                                               standard_concept_2,
                                               standard_concept_3), ~ifelse(is.na(.), "N", .))

                } else if (relationship_type == "mapping") {

                        edges <-
                                queryAthena(
                                        SqlRender::render(
                                                "
                          SELECT DISTINCT
                            v1.concept_name AS concept_class_id_1,
                            'R' AS standard_concept_1,
                            cr.relationship_id,
                            v2.concept_class_id AS concept_class_id_2,
                            v2.standard_concept AS standard_concept_2,
                            v2.concept_name AS concept_name_3,
                            'I' AS standard_concept_3
                          FROM omop_vocabulary.concept_relationship cr
                          INNER JOIN omop_vocabulary.concept v1
                          ON v1.concept_id = cr.concept_id_1
                          INNER JOIN omop_vocabulary.concept v2
                          ON v2.concept_id = cr.concept_id_2
                          WHERE
                           cr.invalid_reason IS NULL
                           AND v1.concept_id = @concept_id
                           AND v1.invalid_reason IS NULL
                           AND v2.invalid_reason IS NULL
                           AND cr.relationship_id IN ('Maps to', 'Mapped from')
                          ",
                                                concept_id = concept_id)) %>%
                                # True NA values do not split out in list so needs to be replaced with N
                                mutate_at(vars(standard_concept_1,
                                               standard_concept_2,
                                               standard_concept_3), ~ifelse(is.na(.), "N", .))

                } else if (relationship_type == "taxonomy") {

                        edges <-
                                queryAthena(
                                        SqlRender::render(
                                                "
                          SELECT DISTINCT
                            v1.concept_name AS concept_class_id_1,
                            'R' AS standard_concept_1,
                            cr.relationship_id,
                            v2.concept_class_id AS concept_class_id_2,
                            v2.standard_concept AS standard_concept_2,
                            v2.concept_name AS concept_name_3,
                            'I' AS standard_concept_3
                          FROM omop_vocabulary.concept_relationship cr
                          INNER JOIN omop_vocabulary.concept v1
                          ON v1.concept_id = cr.concept_id_1
                          INNER JOIN omop_vocabulary.concept v2
                          ON v2.concept_id = cr.concept_id_2
                          WHERE
                           cr.invalid_reason IS NULL
                           AND v1.concept_id = @concept_id
                           AND v1.invalid_reason IS NULL
                           AND v2.invalid_reason IS NULL
                           AND cr.relationship_id IN ('Is a', 'Subsumes')
                          ",
                                                concept_id = concept_id)) %>%
                                # True NA values do not split out in list so needs to be replaced with N
                                mutate_at(vars(standard_concept_1,
                                               standard_concept_2,
                                               standard_concept_3), ~ifelse(is.na(.), "N", .))
                } else if (relationship_type == "lateral") {


                        edges <-
                                queryAthena(
                                        SqlRender::render(
                                                "
                          SELECT DISTINCT
                            v1.concept_name AS concept_class_id_1,
                            'R' AS standard_concept_1,
                            cr.relationship_id,
                            v2.concept_class_id AS concept_class_id_2,
                            v2.standard_concept AS standard_concept_2,
                            v2.concept_name AS concept_name_3,
                            'I' AS standard_concept_3
                          FROM omop_vocabulary.concept_relationship cr
                          INNER JOIN omop_vocabulary.concept v1
                          ON v1.concept_id = cr.concept_id_1
                          INNER JOIN omop_vocabulary.concept v2
                          ON v2.concept_id = cr.concept_id_2
                          WHERE
                           cr.invalid_reason IS NULL
                           AND v1.concept_id = @concept_id
                           AND v1.invalid_reason IS NULL
                           AND v2.invalid_reason IS NULL
                           AND cr.relationship_id NOT IN ('Is a', 'Subsumes', 'Mapped from', 'Maps to')
                          ",
                                                concept_id = concept_id)) %>%
                                # True NA values do not split out in list so needs to be replaced with N
                                mutate_at(vars(standard_concept_1,
                                               standard_concept_2,
                                               standard_concept_3), ~ifelse(is.na(.), "N", .))




                }


                if (nrow(edges) == 0) {

                        stop(sprintf('"%s" and "%s" do not have valid relationships.', vocabulary_id, concept_class_id))
                }

                nodes <-
                        bind_rows(edges %>%
                                          select(ends_with("1")) %>%
                                          rename_all(str_remove_all, "_1$"),
                                  edges %>%
                                          select(ends_with("2")) %>%
                                          rename_all(str_remove_all,"_2$"),
                                  edges %>%
                                          select(ends_with("3")) %>%
                                          rename_all(str_remove_all,"_3$") %>%
                                          rename(concept_class_id = concept_name)) %>%
                        distinct()


                ndf <-
                        create_node_df(n = nrow(nodes),
                                       type = nodes$standard_concept,
                                       label = nodes$concept_class_id,
                                       style = "filled",
                                       fixedsize = TRUE,
                                       fontcolor = "black",
                                       color = "white") %>%
                        mutate(fillcolor =
                                       case_when(type == "C" ~ "blue",
                                                 type == "S" ~ "red",
                                                 type == "N" ~ "gray",
                                                 type == "I" ~  "white",
                                                 type == "R" ~ "green",
                                                 TRUE ~ "white"),
                               shape =
                                       case_when(type == "C" ~ "box",
                                                 type == "I" ~ "freetext",
                                                 type == "R" ~ "diamond",
                                                 TRUE ~ "circle"),
                               fixedsize =
                                       case_when(type == "R" ~ FALSE,
                                                 TRUE ~ fixedsize)
                        )



                edf_a <-
                        edges %>%
                        left_join(ndf,
                                  by = c("standard_concept_1" = "type",
                                         "concept_class_id_1" = "label")) %>%
                        rename(from = id) %>%
                        left_join(ndf,
                                  by = c("standard_concept_2" = "type",
                                         "concept_class_id_2" = "label")) %>%
                        rename(to = id) %>%
                        select(from,
                               to,
                               rel = relationship_id)

                edf_b <-
                        edges %>%
                        left_join(ndf,
                                  by = c("standard_concept_2" = "type",
                                         "concept_class_id_2" = "label")) %>%
                        rename(from = id) %>%
                        left_join(ndf,
                                  by = c("standard_concept_3" = "type",
                                         "concept_name_3" = "label")) %>%
                        rename(to = id) %>%
                        select(from,
                               to)

                edf <-
                        bind_rows(edf_a,
                                  edf_b) %>%
                        distinct()


                edf <-
                        create_edge_df(from = edf$from,
                                       to   = edf$to,
                                       rel  = edf$rel,
                                       label =edf$rel,
                                       len   = 5,
                                       fontsize = 14)


                graph <-
                        DiagrammeR::create_graph(nodes_df = ndf,
                                                 edges_df = edf)

                render_graph(graph)



        }




