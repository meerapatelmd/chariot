print_loinc_class <-
        function(conn,
                 concept_obj,
                 vocab_schema = "omop_vocabulary",
                 color_by = "standard_concept",
                 verbose = TRUE,
                 render_sql = TRUE,
                 sleep_time = 1) {


                if (is.concept(concept_obj)) {

                        concept_id <- concept_obj@concept_id

                } else {

                        stop("`concept_obj` must be a concept class object")

                }

                domain_id <- "Measurement"
                vocabulary_id <- "LOINC"
                child <- domain_id
                root <-
                        tibble::tibble(parent = NA_character_,
                                       child = child)



                level_1 <-
                        queryAthena(sql_statement =
                                            SqlRender::render(
                                                    "
                                WITH ancestry AS (
                                    SELECT DISTINCT ca.ancestor_concept_id, ca.descendant_concept_id
                                    FROM @vocab_schema.concept c
                                    INNER JOIN @vocab_schema.concept_ancestor ca
                                    ON ca.ancestor_concept_id = c.concept_id
                                    INNER JOIN @vocab_schema.concept c2
                                    ON ca.descendant_concept_id = c2.concept_id
                                    WHERE
                                    c.vocabulary_id IN ('@vocabulary_id')
                                    AND c.standard_concept = 'C'
                                    AND c.invalid_reason IS NULL
                                    AND c2.invalid_reason IS NULL
                                    AND c2.standard_concept = 'C'
                                    AND c2.vocabulary_id IN ('@vocabulary_id')
                                    AND c.domain_id = '@domain_id'
                                    AND c2.domain_id = '@domain_id'
                                    AND ca.ancestor_concept_id <> ca.descendant_concept_id
                                    AND ca.min_levels_of_separation = 1 AND ca.max_levels_of_separation = 1
                                )

                            SELECT DISTINCT c.*
                                FROM ancestry a
                            LEFT JOIN @vocab_schema.concept c
                            ON c.concept_id = a.ancestor_concept_id
                            WHERE a.ancestor_concept_id IN (@concept_id);",
                                                    vocab_schema = vocab_schema,
                                                    vocabulary_id = vocabulary_id,
                                                    domain_id = domain_id,
                                                    concept_id = concept_id),
                                    conn = conn,
                                    conn_fun = conn_fun,
                                    skip_cache = TRUE,
                                    render_sql = render_sql,
                                    verbose = verbose,
                                    sleepTime = sleepTime
                        )

                stopifnot(nrow(level_1) > 0)

                level_1 <-
                        level_1 %>%
                        dplyr::mutate(parent = child) %>%
                        tidyr::unite(col = child,
                                     concept_id,
                                     concept_name,
                                     sep = " ",
                                     na.rm = TRUE,
                                     remove = FALSE) %>%
                        dplyr::select(parent,
                                      child,
                                      dplyr::everything())

                range_output <- list()
                range_output[[1]] <- level_1


                range <- 1:100
                for (i in 2:max(range)) {


                        new_parents <-
                                range_output[[i-1]] %>%
                                dplyr::select(concept_id) %>%
                                dplyr::distinct() %>%
                                unlist() %>%
                                as.integer()

                        level_n <-
                                queryAthena(sql_statement =
                                                    SqlRender::render(
                                                            "
                                    WITH ancestry AS (
                                        SELECT DISTINCT ca.ancestor_concept_id, ca.descendant_concept_id
                                        FROM @vocab_schema.concept c
                                        INNER JOIN @vocab_schema.concept_ancestor ca
                                        ON ca.ancestor_concept_id = c.concept_id
                                        INNER JOIN @vocab_schema.concept c2
                                        ON ca.descendant_concept_id = c2.concept_id
                                        WHERE
                                        c.vocabulary_id IN ('@vocabulary_id')
                                        AND c.standard_concept = 'C'
                                        AND c.invalid_reason IS NULL
                                        AND c2.invalid_reason IS NULL
                                        AND c2.standard_concept = 'C'
                                        AND c2.vocabulary_id IN ('@vocabulary_id')
                                        AND c.domain_id = '@domain_id'
                                        AND c2.domain_id = '@domain_id'
                                        AND ca.ancestor_concept_id <> ca.descendant_concept_id
                                        AND ca.min_levels_of_separation = 1 AND ca.max_levels_of_separation = 1
                                    )

                                    SELECT DISTINCT
                                        CONCAT(parent.concept_id, ' ', parent.concept_name) AS parent,
                                        CONCAT(child.concept_id, ' ', child.concept_name) AS child,
                                        child.*
                                    FROM ancestry a
                                    LEFT JOIN @vocab_schema.concept parent
                                    ON a.ancestor_concept_id = parent.concept_id
                                    LEFT JOIN @vocab_schema.concept child
                                    ON a.descendant_concept_id = child.concept_id
                                    WHERE a.ancestor_concept_id IN (@new_parents)
                                    ;",
                                                            vocab_schema = vocab_schema,
                                                            vocabulary_id = vocabulary_id,
                                                            domain_id = domain_id,
                                                            new_parents = new_parents),
                                            conn = conn,
                                            verbose = verbose,
                                            render_sql = render_sql
                                )


                        if (nrow(level_n) == 0) {

                                break()

                        } else {
                                range_output[[i]] <- level_n
                        }

                }

                secretary::typewrite("There are", length(range_output), "levels below", secretary::inside_out(sprintf('%s %s', concept_obj@concept_id, concept_obj@concept_name)))
                secretary::typewrite("Row counts:")
                1:length(range_output) %>%
                        purrr::map2(range_output,
                                    function(x,y)
                                            secretary::typewrite(sprintf("%s: %s", x, nrow(y)), tabs = 4, timepunched = FALSE)
                        )


                range_output
        }













#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param vocabulary_id PARAM_DESCRIPTION
#' @param domain_id PARAM_DESCRIPTION
#' @param conn PARAM_DESCRIPTION
#' @param vocab_schema PARAM_DESCRIPTION
#' @param range PARAM_DESCRIPTION, Default: 1:10
#' @param color_by PARAM_DESCRIPTION, Default: 'vocabulary_id'
#' @param terminal_vocabulary_id PARAM_DESCRIPTION
#' @param render_sql PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[SqlRender]{render}}
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{select}},\code{\link[dplyr]{reexports}},\code{\link[dplyr]{distinct}},\code{\link[dplyr]{bind}},\code{\link[dplyr]{mutate_all}},\code{\link[dplyr]{group_by}},\code{\link[dplyr]{summarise_all}},\code{\link[dplyr]{mutate-joins}}
#'  \code{\link[tidyr]{unite}},\code{\link[tidyr]{pivot_longer}}
#'  \code{\link[colorspace]{rainbow_hcl}}
#'  \code{\link[collapsibleTree]{collapsibleTreeNetwork}}
#' @rdname plot_loinc_classification
#' @export
#' @importFrom tibble tibble
#' @importFrom SqlRender render
#' @importFrom dplyr mutate select everything distinct bind_rows mutate_all group_by summarize_at ungroup left_join
#' @importFrom tidyr unite pivot_longer
#' @importFrom colorspace terrain_hcl
#' @importFrom collapsibleTree collapsibleTreeNetwork

plot_loinc_classification <-
        function(conn,
                 concept_obj,
                 vocab_schema = "omop_vocabulary",
                 color_by = "standard_concept",
                 verbose = TRUE,
                 render_sql = TRUE,
                 sleep_time = 1) {


                if (is.concept(concept_obj)) {

                        concept_id <- concept_obj@concept_id

                } else {

                        stop("`concept_obj` must be a concept class object")

                }

                domain_id <- "Measurement"
                vocabulary_id <- "LOINC"
                child <- domain_id
                root <-
                        tibble::tibble(parent = NA_character_,
                                       child = child)



                level_1 <-
                        queryAthena(sql_statement =
                                            SqlRender::render(
                                                    "
                                WITH ancestry AS (
                                    SELECT DISTINCT ca.ancestor_concept_id, ca.descendant_concept_id
                                    FROM @vocab_schema.concept c
                                    INNER JOIN @vocab_schema.concept_ancestor ca
                                    ON ca.ancestor_concept_id = c.concept_id
                                    INNER JOIN @vocab_schema.concept c2
                                    ON ca.descendant_concept_id = c2.concept_id
                                    WHERE
                                    c.vocabulary_id IN ('@vocabulary_id')
                                    AND c.standard_concept = 'C'
                                    AND c.invalid_reason IS NULL
                                    AND c2.invalid_reason IS NULL
                                    AND c2.standard_concept = 'C'
                                    AND c2.vocabulary_id IN ('@vocabulary_id')
                                    AND c.domain_id = '@domain_id'
                                    AND c2.domain_id = '@domain_id'
                                    AND ca.ancestor_concept_id <> ca.descendant_concept_id
                                    AND ca.min_levels_of_separation = 1 AND ca.max_levels_of_separation = 1
                                )

                            SELECT DISTINCT c.*
                                FROM ancestry a
                            LEFT JOIN @vocab_schema.concept c
                            ON c.concept_id = a.ancestor_concept_id
                            WHERE a.ancestor_concept_id IN (@concept_id);",
                                                    vocab_schema = vocab_schema,
                                                    vocabulary_id = vocabulary_id,
                                                    domain_id = domain_id,
                                                    concept_id = concept_id),
                                    conn = conn,
                                    conn_fun = conn_fun,
                                    skip_cache = TRUE,
                                    render_sql = render_sql,
                                    verbose = verbose,
                                    sleepTime = sleepTime
                        )

                stopifnot(nrow(level_1) > 0)

                level_1 <-
                        level_1 %>%
                        dplyr::mutate(parent = child) %>%
                        tidyr::unite(col = child,
                                     concept_id,
                                     concept_name,
                                     sep = " ",
                                     na.rm = TRUE,
                                     remove = FALSE) %>%
                        dplyr::select(parent,
                                      child,
                                      dplyr::everything())

                range_output <- list()
                range_output[[1]] <- level_1


                range <- 1:100
                for (i in 2:max(range)) {


                        new_parents <-
                                range_output[[i-1]] %>%
                                dplyr::select(concept_id) %>%
                                dplyr::distinct() %>%
                                unlist() %>%
                                as.integer()

                        level_n <-
                                queryAthena(sql_statement =
                                                    SqlRender::render(
                                                            "
                                    WITH ancestry AS (
                                        SELECT DISTINCT ca.ancestor_concept_id, ca.descendant_concept_id
                                        FROM @vocab_schema.concept c
                                        INNER JOIN @vocab_schema.concept_ancestor ca
                                        ON ca.ancestor_concept_id = c.concept_id
                                        INNER JOIN @vocab_schema.concept c2
                                        ON ca.descendant_concept_id = c2.concept_id
                                        WHERE
                                        c.vocabulary_id IN ('@vocabulary_id')
                                        AND c.standard_concept = 'C'
                                        AND c.invalid_reason IS NULL
                                        AND c2.invalid_reason IS NULL
                                        AND c2.standard_concept = 'C'
                                        AND c2.vocabulary_id IN ('@vocabulary_id')
                                        AND c.domain_id = '@domain_id'
                                        AND c2.domain_id = '@domain_id'
                                        AND ca.ancestor_concept_id <> ca.descendant_concept_id
                                        AND ca.min_levels_of_separation = 1 AND ca.max_levels_of_separation = 1
                                    )

                                    SELECT DISTINCT
                                        CONCAT(parent.concept_id, ' ', parent.concept_name) AS parent,
                                        CONCAT(child.concept_id, ' ', child.concept_name) AS child,
                                        child.*
                                    FROM ancestry a
                                    LEFT JOIN @vocab_schema.concept parent
                                    ON a.ancestor_concept_id = parent.concept_id
                                    LEFT JOIN @vocab_schema.concept child
                                    ON a.descendant_concept_id = child.concept_id
                                    WHERE a.ancestor_concept_id IN (@new_parents)
                                    ;",
                                                            vocab_schema = vocab_schema,
                                                            vocabulary_id = vocabulary_id,
                                                            domain_id = domain_id,
                                                            new_parents = new_parents),
                                            conn = conn,
                                            verbose = verbose,
                                            render_sql = render_sql
                                )


                        if (nrow(level_n) == 0) {

                                break()

                        } else {
                                range_output[[i]] <- level_n
                        }

                }

                secretary::typewrite("There are", length(range_output), "levels below", secretary::inside_out(sprintf('%s %s', concept_obj@concept_id, concept_obj@concept_name)))
                1:length(range_output) %>%
                        purrr::map2(range_output,
                                    function(x,y)
                                            secretary::typewrite(sprintf("%s: %s", x, nrow(y)), tabs = 4, timepunched = FALSE)
                                            )




                df <- dplyr::bind_rows(root,
                                       range_output)

                tooltip <-
                        df %>%
                        dplyr::mutate_all(as.character) %>%
                        tidyr::pivot_longer(cols = !c(parent,child),
                                            names_to = "attribute",
                                            values_to = "attribute_value",
                                            values_drop_na = TRUE) %>%
                        tidyr::unite(col = tooltip,
                                     attribute,
                                     attribute_value,
                                     sep = ": ",
                                     remove = TRUE,
                                     na.rm = TRUE) %>%
                        dplyr::distinct() %>%
                        dplyr::group_by(child) %>%
                        dplyr::summarize_at(dplyr::vars(tooltip), ~paste(., collapse = "<br>")) %>%
                        dplyr::ungroup() %>%
                        dplyr::distinct()

                color <- unlist(df[,color_by])
                color[is.na(color)] <- "NA"
                df$color <- factor(color)
                levels(df$color) <- colorspace::terrain_hcl(n = length(levels(df$color)))
                df$color <- as.character(df$color)

                df <-
                        df %>%
                        dplyr::select(parent, child, color) %>%
                        dplyr::left_join(tooltip) %>%
                        dplyr::distinct()

                secretary::typewrite("There are", nrow(df), "rows in the data tree. Plot? ")
                secretary::press_enter()

                collapsibleTree::collapsibleTreeNetwork(df = df,
                                                        tooltipHtml = "tooltip",
                                                        fill = "color")

        }
