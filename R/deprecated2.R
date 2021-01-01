#' @title
#' Get the Top Classes
#'
#' @description
#' Get the top classes in the Concept Ancestor table by vocabulary with the option of filtering further for domain.
#'
#' @noRd

lookup_top_classes <-
        function(vocabulary_id,
                 domain_id,
                 vocab_schema = "omop_vocabulary",
                 conn,
                 conn_fun = "connectAthena()",
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 cache_resultset = TRUE,
                 render_sql = TRUE,
                 verbose = TRUE,
                 sleepTime = 1) {


                if (missing(domain_id)) {

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
                                    AND ca.ancestor_concept_id <> ca.descendant_concept_id
                                    AND ca.min_levels_of_separation = 1 AND ca.max_levels_of_separation = 1
                                )

                            SELECT DISTINCT c.*
                                FROM ancestry a
                            LEFT JOIN @vocab_schema.concept c
                            ON c.concept_id = a.ancestor_concept_id
                            WHERE a.ancestor_concept_id NOT IN (
                                SELECT a2.descendant_concept_id
                                FROM ancestry a2);",
                                                    vocab_schema = vocab_schema,
                                                    vocabulary_id = vocabulary_id),
                                    conn = conn,
                                    conn_fun = conn_fun,
                                    cache_only = cache_only,
                                    skip_cache = skip_cache,
                                    override_cache = override_cache,
                                    cache_resultset = cache_resultset,
                                    render_sql = render_sql,
                                    verbose = verbose,
                                    sleepTime = sleepTime
                        )



                } else {

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
                            WHERE a.ancestor_concept_id NOT IN (
                                SELECT a2.descendant_concept_id
                                FROM ancestry a2);",
                                                    vocab_schema = vocab_schema,
                                                    vocabulary_id = vocabulary_id,
                                                    domain_id = domain_id),
                                    conn = conn,
                                    conn_fun = conn_fun,
                                    cache_only = cache_only,
                                    skip_cache = skip_cache,
                                    override_cache = override_cache,
                                    cache_resultset = cache_resultset,
                                    render_sql = render_sql,
                                    verbose = verbose,
                                    sleepTime = sleepTime
                        )
                }
        }






#' @title
#' Vuew ATC Class Descendants
#'
#' @description
#' Due to the high amount of records, used to determine the appropriate range based on row counts per level to supply the optional `range` argument for \code{\link{plot_loinc_classification}}, where this fucntion is called again and can be optionally filtered on a numeric range before plotting.
#'
#' @noRd
#' @rdname preview_atc_classification

preview_atc_classification <-
        function(conn,
                 concept_class_obj,
                 vocab_schema = "omop_vocabulary",
                 verbose = TRUE,
                 render_sql = TRUE,
                 sleep_time = 1) {


                if (is.concept(concept_class_obj)) {

                        concept_id <- concept_class_obj@concept_id

                } else {

                        stop("`concept_class_obj` must be a concept class object")

                }

                domain_id <- "Drug"
                vocabulary_id <- "ATC"
                child <- domain_id



                level_1 <<-
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
                                        --AND c2.standard_concept = 'C'
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

                secretary::typewrite("There are", length(range_output), "levels below", secretary::inside_out(sprintf('%s %s', concept_class_obj@concept_id, concept_class_obj@concept_name)))
                secretary::typewrite("Row counts:")
                1:length(range_output) %>%
                        purrr::map2(range_output,
                                    function(x,y)
                                            secretary::typewrite(sprintf("%s: %s", x, nrow(y)), tabs = 4, timepunched = FALSE)
                        )


                range_output
        }



#' @title
#' Plot a LOINC Class
#'
#' @description
#' Plot a LOINC Class. Due to the high amount of records, use \code{\link{loinc_classification}} to determine the appropriate range based on row counts per level to supply the optional `range` argument.
#'
#' @param skip_plot If true, returns the dataframe before it is plotted and plotting is not done. This is an option to troubleshoot or customize a plot beyond what is available within the function.
#' @seealso
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[dplyr]{bind}},\code{\link[dplyr]{mutate_all}},\code{\link[dplyr]{distinct}},\code{\link[dplyr]{group_by}},\code{\link[dplyr]{summarise_all}},\code{\link[dplyr]{vars}},\code{\link[dplyr]{select}},\code{\link[dplyr]{mutate-joins}}
#'  \code{\link[tidyr]{pivot_longer}},\code{\link[tidyr]{unite}}
#'  \code{\link[colorspace]{rainbow_hcl}}
#'  \code{\link[secretary]{c("typewrite", "typewrite")}},\code{\link[secretary]{press_enter}}
#'  \code{\link[collapsibleTree]{collapsibleTreeNetwork}}
#' @rdname plot_atc_classification
#' @noRd
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows mutate_all distinct group_by summarize_at vars ungroup select left_join
#' @importFrom tidyr pivot_longer unite
#' @importFrom colorspace terrain_hcl
#' @importFrom secretary typewrite press_enter
#' @importFrom collapsibleTree collapsibleTreeNetwork
#' @importFrom htmlwidgets saveWidget

plot_atc_classification <-
        function(conn,
                 concept_class_obj,
                 range,
                 file,
                 vocab_schema = "omop_vocabulary",
                 color_by = "standard_concept",
                 verbose = TRUE,
                 render_sql = TRUE,
                 sleep_time = 1,
                 skip_plot = FALSE) {


                if (is.concept(concept_class_obj)) {

                        concept_id <- concept_class_obj@concept_id

                } else {

                        stop("`concept_class_obj` must be a concept class object")

                }

                domain_id <- "Measurement"
                vocabulary_id <- "LOINC"
                child <- domain_id
                root <-
                        tibble::tibble(parent = NA_character_,
                                       child = child)


                range_output <- preview_atc_classification(conn = conn,
                                                        concept_class_obj = concept_class_obj,
                                                        vocab_schema = vocab_schema,
                                                        verbose = verbose,
                                                        render_sql = render_sql,
                                                        sleep_time = sleep_time)

                if (!missing(range)) {

                        range_output <- range_output[range]
                }

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

                if (skip_plot) {

                        df

                } else {
                secretary::typewrite("There are", nrow(df), "rows in the data tree. Plotting...")

                if (missing(file)) {

                        collapsibleTree::collapsibleTreeNetwork(df = df,
                                                                tooltipHtml = "tooltip",
                                                                fill = "color")

                } else {

                        p <- collapsibleTree::collapsibleTreeNetwork(df = df,
                                                                tooltipHtml = "tooltip",
                                                                fill = "color")

                        htmlwidgets::saveWidget(widget = p,
                                                file = file)
                }
                }

        }





#' @title
#' Preview Concept Lineage
#'
#' @noRd
#' @rdname preview_concept_classification

preview_rxnorm_atc_classification <-
        function(conn,
                 rxnorm_concept_obj,
                 vocab_schema = "omop_vocabulary",
                 verbose = TRUE,
                 render_sql = TRUE,
                 sleep_time = 1) {


                if (is.concept(concept_obj)) {

                        concept_id <- concept_obj@concept_id

                } else {

                        stop("`concept_obj` must be a concept class object")

                }

                domain_id <- "Drug"
                vocabulary_id <- "ATC"
                child <- domain_id



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
                                    c.vocabulary_id IN ('ATC', 'RxNorm', 'RxNorm Extension')
                                    AND c.standard_concept = 'C'
                                    AND c.invalid_reason IS NULL
                                    AND c2.invalid_reason IS NULL
                                    -- AND c2.standard_concept = 'C'
                                    AND c2.vocabulary_id IN ('ATC', 'RxNorm', 'RxNorm Extension')
                                    AND c.domain_id = 'Drug'
                                    AND c2.domain_id = 'Drug'
                                    AND ca.ancestor_concept_id <> ca.descendant_concept_id
                                    -- AND ca.min_levels_of_separation = 1 AND ca.max_levels_of_separation = 1
                                )

                            SELECT DISTINCT c.*
                                FROM ancestry a
                            LEFT JOIN @vocab_schema.concept c
                            ON c.concept_id = a.ancestor_concept_id
                            WHERE a.descendant_concept_id IN (@concept_id)
                                                     AND c.concept_class_id = 'ATC 1st';",
                                                    vocab_schema = vocab_schema,
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
                                        c.vocabulary_id IN ('ATC', 'RxNorm', 'RxNorm Extension')
                                        AND c.standard_concept = 'C'
                                        AND c.invalid_reason IS NULL
                                        AND c2.invalid_reason IS NULL
                                        --AND c2.standard_concept = 'C'
                                        AND c2.vocabulary_id IN ('ATC', 'RxNorm', 'RxNorm Extension')
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

                secretary::typewrite("There are", length(range_output), "levels above", secretary::inside_out(sprintf('%s %s', concept_obj@concept_id, concept_obj@concept_name)))
                secretary::typewrite("Row counts:")
                1:length(range_output) %>%
                        purrr::map2(range_output,
                                    function(x,y)
                                            secretary::typewrite(sprintf("%s: %s", x, nrow(y)), tabs = 4, timepunched = FALSE)
                        )


                range_output
        }



#' @title
#' Plot a LOINC Class
#'
#' @description
#' Plot a LOINC Class. Due to the high amount of records, use \code{\link{loinc_classification}} to determine the appropriate range based on row counts per level to supply the optional `range` argument.
#'
#' @seealso
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[dplyr]{bind}},\code{\link[dplyr]{mutate_all}},\code{\link[dplyr]{distinct}},\code{\link[dplyr]{group_by}},\code{\link[dplyr]{summarise_all}},\code{\link[dplyr]{vars}},\code{\link[dplyr]{select}},\code{\link[dplyr]{mutate-joins}}
#'  \code{\link[tidyr]{pivot_longer}},\code{\link[tidyr]{unite}}
#'  \code{\link[colorspace]{rainbow_hcl}}
#'  \code{\link[secretary]{c("typewrite", "typewrite")}},\code{\link[secretary]{press_enter}}
#'  \code{\link[collapsibleTree]{collapsibleTreeNetwork}}
#' @rdname plot_atc_classification
#' @noRd
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows mutate_all distinct group_by summarize_at vars ungroup select left_join
#' @importFrom tidyr pivot_longer unite
#' @importFrom colorspace terrain_hcl
#' @importFrom secretary typewrite press_enter
#' @importFrom collapsibleTree collapsibleTreeNetwork
#' @importFrom htmlwidgets saveWidget

plot_atc_classification <-
        function(conn,
                 concept_obj,
                 range,
                 file,
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


                range_output <- preview_atc_classification(conn = conn,
                                                        concept_obj = concept_obj,
                                                        vocab_schema = vocab_schema,
                                                        verbose = verbose,
                                                        render_sql = render_sql,
                                                        sleep_time = sleep_time)

                if (!missing(range)) {

                        range_output <- range_output[range]
                }

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

                secretary::typewrite("There are", nrow(df), "rows in the data tree. Plotting...")

                if (missing(file)) {

                        collapsibleTree::collapsibleTreeNetwork(df = df,
                                                                tooltipHtml = "tooltip",
                                                                fill = "color")

                } else {

                        p <- collapsibleTree::collapsibleTreeNetwork(df = df,
                                                                tooltipHtml = "tooltip",
                                                                fill = "color")

                        htmlwidgets::saveWidget(widget = p,
                                                file = file)
                }

        }





#' @title
#' View LOINC Class Descendants
#'
#' @description
#' Due to the high amount of records, used to determine the appropriate range based on row counts per level to supply the optional `range` argument for \code{\link{plot_loinc_classification}}, where this fucntion is called again and can be optionally filtered on a numeric range before plotting.
#'
#' @noRd
#' @rdname preview_loinc_classification

preview_loinc_classification <-
        function(conn,
                 concept_class_obj,
                 vocab_schema = "omop_vocabulary",
                 verbose = TRUE,
                 render_sql = TRUE,
                 sleep_time = 1) {


                if (is.concept(concept_class_obj)) {

                        concept_id <- concept_class_obj@concept_id

                } else {

                        stop("`concept_class_obj` must be a concept class object")

                }

                domain_id <- "Measurement"
                vocabulary_id <- "LOINC"
                child <- domain_id



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
                                        --AND c2.standard_concept = 'C'
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

                secretary::typewrite("There are", length(range_output), "levels below", secretary::inside_out(sprintf('%s %s', concept_class_obj@concept_id, concept_class_obj@concept_name)))
                secretary::typewrite("Row counts:")
                1:length(range_output) %>%
                        purrr::map2(range_output,
                                    function(x,y)
                                            secretary::typewrite(sprintf("%s: %s", x, nrow(y)), tabs = 4, timepunched = FALSE)
                        )


                range_output
        }



#' @title
#' Plot a LOINC Class
#'
#' @description
#' Plot a LOINC Class. Due to the high amount of records, use \code{\link{preview_loinc_classification}} to determine the appropriate range based on row counts per level to supply the optional `range` argument.
#' @param skip_plot If true, returns the dataframe before it is plotted and plotting is not done. This is an option to troubleshoot or customize a plot beyond what is available within the function.
#'
#' @seealso
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[dplyr]{bind}},\code{\link[dplyr]{mutate_all}},\code{\link[dplyr]{distinct}},\code{\link[dplyr]{group_by}},\code{\link[dplyr]{summarise_all}},\code{\link[dplyr]{vars}},\code{\link[dplyr]{select}},\code{\link[dplyr]{mutate-joins}}
#'  \code{\link[tidyr]{pivot_longer}},\code{\link[tidyr]{unite}}
#'  \code{\link[colorspace]{rainbow_hcl}}
#'  \code{\link[secretary]{c("typewrite", "typewrite")}},\code{\link[secretary]{press_enter}}
#'  \code{\link[collapsibleTree]{collapsibleTreeNetwork}}
#' @rdname plot_loinc_classification
#' @noRd
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows mutate_all distinct group_by summarize_at vars ungroup select left_join
#' @importFrom tidyr pivot_longer unite
#' @importFrom colorspace terrain_hcl
#' @importFrom secretary typewrite press_enter
#' @importFrom collapsibleTree collapsibleTreeNetwork
#' @importFrom htmlwidgets saveWidget

plot_loinc_classification <-
        function(conn,
                 concept_class_obj,
                 range,
                 file,
                 vocab_schema = "omop_vocabulary",
                 color_by = "standard_concept",
                 skip_plot = FALSE,
                 verbose = TRUE,
                 render_sql = TRUE,
                 sleep_time = 1) {


                if (is.concept(concept_class_obj)) {

                        concept_id <- concept_class_obj@concept_id

                } else {

                        stop("`concept_class_obj` must be a concept class object")

                }

                domain_id <- "Measurement"
                vocabulary_id <- "LOINC"
                child <- domain_id
                root <-
                        tibble::tibble(parent = NA_character_,
                                       child = child)


                range_output <- preview_loinc_classification(conn = conn,
                                                        concept_class_obj = concept_class_obj,
                                                        vocab_schema = vocab_schema,
                                                        verbose = verbose,
                                                        render_sql = render_sql,
                                                        sleep_time = sleep_time)

                if (!missing(range)) {

                        range_output <- range_output[range]
                }

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


                if (skip_plot) {

                        df
                } else {

                secretary::typewrite("There are", nrow(df), "rows in the data tree. Plotting... ")

                if (missing(file)) {

                        collapsibleTree::collapsibleTreeNetwork(df = df,
                                                                tooltipHtml = "tooltip",
                                                                fill = "color")

                } else {

                        p <- collapsibleTree::collapsibleTreeNetwork(df = df,
                                                                tooltipHtml = "tooltip",
                                                                fill = "color")

                        htmlwidgets::saveWidget(widget = p,
                                                file = file)
                }
                }

        }





#' @title =
#' Get Strip
#' @example inst/example/format.R
#' @seealso
#'  \code{\link[dplyr]{select}}
#' @rdname getStrip
#' @noRd
#' @importFrom dplyr select

getStrip <-
    function(concept_id,
             schema = "public") {

        queryConceptId(concept_ids = concept_id,
                       schema = schema) %>%
            mergeStrip(into = "Concept") %>%
            dplyr::select("Concept") %>%
            unlist() %>%
            unname()

    }

#' @title
#' Unbox Strip
#' @description
#' First separates rows by the `row_sep` argument, followed by unmerging the strip
#' @seealso
#'  \code{\link[tidyr]{separate_rows}}
#' @rdname unboxStrip
#' @noRd
#' @importFrom tidyr separate_rows


unboxStrip <-
    function(data,
             strip_col,
             row_sep = "\n",
             remove = FALSE) {


            # test_data <-
            #     tibble::tibble(Concept = "[V] [S] 1112807 aspirin [RxNorm 1191] [Drug] [Ingredient]\n[V] [S] 1112807 aspirin [RxNorm 1191] [Drug] [Ingredient]")

            data %>%
                 tidyr::separate_rows({{ strip_col }}, sep = row_sep) %>%
                    unmergeStrip(strip_col = {{ strip_col }},
                                 remove = remove)



    }


#' @title
#' Unbox Strip
#' @description
#' First separates rows by the `row_sep` argument, followed by unmerging the strip
#' @seealso
#'  \code{\link[tidyr]{separate_rows}}
#' @rdname unboxLabel
#' @noRd
#' @importFrom tidyr separate_rows extract


unboxLabel <-
    function(data,
             label_col,
             row_sep = "\n",
             remove = FALSE) {


        # test_data <-
        #     tibble::tibble(Concept = "1112807 aspirin\n1112807 aspirin")

        data %>%
            tidyr::separate_rows({{ label_col }}, sep = row_sep) %>%
            tidyr::extract(col = {{ label_col }},
                           into = c("concept_id", "concept_name"),
                           regex = "(^.*?) (.*$)",
                           remove = remove)



    }


#' @title Filter Multiple Concept Strip Columns
#' @description
#' This function performs the same style of filtering as \code{\link{filterStrip}} over multiple columns.
#'
#' @param data         Dataframe
#' @param merge_cols    Character vector of column names to filter
#' @param all           Equivalent to the `all_vars()` variable predicate in the Tidyverse system. If TRUE, all `merge_cols` are filtered for. If FALSE, the equivalent to `any_vars()` is performed. Default: TRUE
#' @param ...           Filter arguments passed to the dplyr filter function using the base Concept Table fields.
#' @return
#' A tibble.
#' @seealso
#' \code{\link{filterStrip}}
#' \code{\link[dplyr]{select}},\code{\link[dplyr]{select_all}},\code{\link[dplyr]{bind}},\code{\link[dplyr]{filter_all}},\code{\link[dplyr]{filter}},\code{\link[dplyr]{distinct}}
#'  \code{\link[tidyr]{separate_rows}}
#'  \code{\link[rubix]{normalize_all_to_na}}
#' @rdname filterAtStrip
#' @noRd
#' @importFrom magrittr %>%
#' @importFrom dplyr select rename_at bind_cols filter_at filter distinct bind_rows
#' @importFrom tidyr separate_rows
#' @importFrom tibble rowid_to_column
#' @importFrom rubix normalize_all_to_na


filterAtStrip <-
        function(data,
                 merge_cols,
                 all = TRUE,
                 ...) {


                data <-
                        data %>%
                        tibble::rowid_to_column("filterAtStripId")


                reserveData <-
                        data

                inputData <-
                        data %>%
                        dplyr::select(filterAtStripId,
                                      all_of(merge_cols)) %>%
                        tidyr::pivot_longer(cols = !filterAtStripId,
                                            names_to = "merge_col",
                                            values_to = "Concept",
                                            values_drop_na = TRUE)

                inputData2 <-
                        inputData %>%
                        separateConceptStrip(Concept) %>%
                        tibble::rowid_to_column("filterAtStripId2")

                inputData3 <-
                        inputData2 %>%
                        unmergeStrip(strip_col = Concept)

                inputData4 <-
                        inputData3 %>%
                        dplyr::filter(...)


                if (all) {

                        inputData5 <-
                                inputData4 %>%
                                dplyr::select(filterAtStripId,
                                              merge_col,
                                              concept_id) %>%
                                dplyr::distinct() %>%
                                dplyr::filter(!is.na(concept_id)) %>%
                                tidyr::pivot_wider(
                                                   id_cols = filterAtStripId,
                                                   names_from = merge_col,
                                                   values_from = concept_id,
                                                   values_fn = list(concept_id = function(x) length(unique(x)))) %>%
                                dplyr::filter_at(dplyr::vars(!filterAtStripId), dplyr::all_vars(!is.na(.)))


                        return(reserveData[(reserveData$filterAtStripId %in% inputData5$filterAtStripId),] %>%
                                        dplyr::select(-contains("filterAtStripId")))

                } else {

                        return(
                        reserveData[(reserveData$filterAtStripId %in% inputData4$filterAtStripId),] %>%
                                dplyr::select(-contains("filterAtStripId")))


                }

                # column_names <-  c("concept_id",
                #                    "concept_name",
                #                    "domain_id",
                #                    "vocabulary_id",
                #                    "concept_class_id",
                #                    "standard_concept",
                #                    "concept_code",
                #                    "valid_start_date",
                #                    "valid_end_date",
                #                    "invalid_reason")

                # if (all) {
                #         for (i in 1:length(merge_cols)) {
                #
                #                 tcol <- merge_cols[i]
                #                 tmp_col <- paste0(tcol, "_tmp")
                #
                #                 tmp_data <-
                #                         data %>%
                #                         dplyr::select(!!tcol) %>%
                #                         dplyr::rename_at(dplyr::vars(1), ~paste(tmp_col))
                #
                #                 data <-
                #                         data %>%
                #                         dplyr::bind_cols(tmp_data) %>%
                #                         tidyr::separate_rows(!!tmp_col,
                #                                              sep = "\n") %>%
                #                         rubix::normalize_all_to_na() %>%
                #                         dplyr::filter_at(dplyr::vars(!!tmp_col), dplyr::all_vars(!is.na(.))) %>%
                #                         unmergeStrip(strip_col = !!tmp_col,
                #                                      remove = FALSE) %>%
                #                         dplyr::filter(...)  %>%
                #                         dplyr::select(-any_of(column_names)) %>%
                #                         dplyr::select(-!!tmp_col) %>%
                #                         dplyr::distinct()
                #
                #                 if (nrow(data) == 0) {
                #                         return(data)
                #                 }
                #
                #         }
                #
                #         return(data)
                #
                # } else {
                #         .output <- list()
                #         for (i in 1:length(merge_cols)) {
                #
                #                 tcol <- merge_cols[i]
                #                 tmp_col <- paste0(tcol, "_tmp")
                #
                #                 tmp_data <-
                #                         data %>%
                #                         dplyr::select(!!tcol) %>%
                #                         dplyr::rename_at(dplyr::vars(1), ~paste(tmp_col))
                #
                #                 .output[[i]] <-
                #                         data %>%
                #                         dplyr::bind_cols(tmp_data) %>%
                #                         tidyr::separate_rows(!!tmp_col,
                #                                              sep = "\n") %>%
                #                         rubix::normalize_all_to_na() %>%
                #                         dplyr::filter_at(dplyr::vars(!!tmp_col), dplyr::all_vars(!is.na(.))) %>%
                #                         unmergeStrip(strip_col = !!tmp_col,
                #                                      remove = FALSE) %>%
                #                         dplyr::filter(...)  %>%
                #                         dplyr::select(-any_of(column_names)) %>%
                #                         dplyr::select(-!!tmp_col) %>%
                #                         dplyr::distinct()
                #
                #         }
                #         .output <- dplyr::bind_rows(.output) %>%
                #                                 dplyr::distinct()
                #         return(.output)
                #
                #
                # }

        }




#' @title  Filter Columns with Merged Concept Strips
#' @description
#' This function filters a column that contains Concept Strips using Concept Table parameters. The target column may contain 1 or more merged concept strip, and the multiple strips must be separated by a new line \"\\n\" for the filter to operate correctly. It is important to note that the the filter is applied to the entire Concept Strip cell and will not alter the data content within the cell otherwise. For example, if the filter `vocabulary_id == 'RxNorm'` is used for `ColumnA`, a `ColumnA` cell that contains at least 1 RxNorm concept will be filtered for though there are other non-RxNorm concepts in that same cell.
#'
#' @param data         dataframe with the merged concept column
#' @param merge_col     column of merged concepts
#' @param ...           arguments that will be passed to the dplyr filter function using the base Concept Table field names
#'
#' @return
#' A tibble with the same number of columns as the input with the number of rows equal or less than that of the input.
#'
#' @details
#' This function:
#' 1. Mutates a copy `merge_col` to a `merge_col_tmp` column,
#' 1. Separates the rows in `merge_col_tmp` by \"\\n\",
#' 1. Filters out any blanks and `NA` values introduced by the row separation,
#' 1. Unmerges the `merge_col` into the base Concept Table field names,
#' 1. Applies the filters found in the ellipses argument,
#' 1. Removes the `merge_col_tmp` and base Concept Table columns to reconstitute the original input, and
#' 1. Removes duplicate rows
#'
#' @seealso
#'  \code{\link[dplyr]{tidyeval-compat}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{filter_all}},\code{\link[dplyr]{filter}},\code{\link[dplyr]{select}},\code{\link[dplyr]{distinct}}
#'  \code{\link[rlang]{as_name}}
#'  \code{\link[tidyr]{separate_rows}}
#'  \code{\link[rubix]{normalize_all_to_na}}
#' @rdname filterStrip
#' @noRd
#' @importFrom magrittr %>%
#' @importFrom dplyr enquo mutate filter_at filter select distinct
#' @importFrom rlang as_name
#' @importFrom tidyr separate_rows
#' @importFrom rubix normalize_all_to_na

filterStrip <-
    function(data,
             merge_col,
             ...) {

            merge_col <- dplyr::enquo(merge_col)
            tmp_col <- paste0(rlang::as_name(merge_col), "tmp")


            column_names <-  c("concept_id",
                                      "concept_name",
                                      "domain_id",
                                      "vocabulary_id",
                                      "concept_class_id",
                                      "standard_concept",
                                      "concept_code",
                                      "valid_start_date",
                                      "valid_end_date",
                                      "invalid_reason")


            if (any(column_names %in% colnames(data))) {

                    qa <- column_names[column_names %in% colnames(data)]

                    stop('data cannot have any concept table column names: ', paste(qa, collapse = ", "))

            }

            .output <-
            data %>%
                dplyr::mutate(!!tmp_col := !!merge_col) %>%
                separateConceptStrip(!!tmp_col) %>%
                # tidyr::separate_rows(!!tmp_col,
                #                      sep = "\n") %>%
                rubix::normalize_all_to_na() %>%
                dplyr::filter_at(dplyr::vars(!!tmp_col), dplyr::all_vars(!is.na(.))) %>%
                unmergeStrip(strip_col = !!tmp_col,
                             remove = FALSE) %>%
                dplyr::filter(...) %>%
                dplyr::select(-any_of(column_names)) %>%
                dplyr::select(-!!tmp_col) %>%
                dplyr::distinct()

            qa <- nrow(.output) > nrow(data)

            if (qa) {
                    warning('returned data has more rows than input data')
            }

            return(.output)

    }




#' Get Merged Concept Id
#' @importFrom dplyr select
#' @noRd


getLabel <-
    function(concept_id,
             schema = "public") {
            queryConceptId(concept_ids = concept_id,
                           schema = schema) %>%
            makeLabel(into = "Label",
                      remove = TRUE) %>%
            dplyr::select(Label) %>%
            unlist() %>%
            unname()
    }




#' Concert a Label Column to a Merge Column
#' @import dplyr
#' @import tidyr
#' @import rubix
#' @noRd

labelToStrip <-
        function(data,
                 label_col,
                 into,
                 remove = FALSE) {

                label_col <- enquo(label_col)
                into <- enquo(into)

                data %>%
                        tidyr::extract(col = !!label_col,
                                        into = c("concept_id", "concept_name"),
                                        regex = "(^.*?) (.*$)",
                                        remove = remove) %>%
                        dplyr::rename(label_concept_id = concept_id) %>%
                        left_join_concept(column = "label_concept_id",
                                          include_synonyms = FALSE)  %>%
                        merge_concepts(into = !!into)

        }




#' Make Label Column
#' @description A Label is in the format of "{concept_id} concept_name". It is less comprehensive than a merged strip using the merge_concepts function, but more human readable when interfacing with others.
#' @importFrom tidyr unite
#' @import dplyr
#' @noRd

makeLabel <-
        function(data,
                 into,
                 prefix = NULL,
                 suffix = NULL,
                 remove = FALSE) {


                label_parts <- paste0(prefix, c("concept_id", "concept_name"), suffix)
                names(label_parts) <- c("concept_id", "concept_name")


                data %>%
                        tidyr::unite(col = {{into}},
                                     dplyr::all_of(label_parts$concept_id),
                                     dplyr::all_of(label_parts$concept_name),
                                     sep = " ",
                                     na.rm = TRUE,
                                     remove = remove) %>%
                        dplyr::mutate_at(dplyr::vars({{into}}), ~na_if(., "NA NA"))
        }




#' Merge OMOP Concepts into a Strip
#' @description This function takes a set of the OMOP Vocabulary Concept Table fields and merges all of them except for the date fields into a single Concept "Strip". If the Strip output is `<NA>` while the input concept id is not, a flagMergeStrip object is returned in the Global Environment.
#' @return A tibble with all blank and "NA" normalized to `<NA>` with 1. If present, `valid_start_date` and `valid_end_date` fields are permanently removed, 2. 8 out of the 10 remaining Concept Table fields (concept_id, concept_name, domain_id, vocabulary_id, concept_class_id, standard_concept, concept_code, invalid_reason) are merged into a single column with the provided column name, 3. the concept_id column is renamed to the format of the provided merged column name: {into_}concept_id. The remaining of the 7 Concept Table fields may also be preserved outside of the merge if provided. All other columns present in the input data are returned along with the transformations described.
#' @param data dataframe with the following required fields from the output
#' @param into name of the column that the new combined string will be in
#' @param ... columns other than concept_id that will be removed in tidyr unite but should be preserved in addition to be merged.
#' @param suffix if the omop concept element column names are different from the standard by a suffix, include it so it can point to the correct set of columns
#' @param prefix if the omop concept element column names are prefixed, include it so it can point to the correct set of columns
#' @param remove If TRUE, remove any possible Concept Table fields in the output, leaving only the newly created label field. All other fields will stay in the output.
#' @import dplyr
#' @import tidyr
#' @importFrom tibble as_tibble
#' @noRd

mergeLabel <-
            function(data,
                     into,
                     suffix = NULL,
                     prefix = NULL,
                     remove = TRUE) {


                                # Enquo output column name
                                into <- dplyr::enquo(into)


                                # Generating a list of concept table columns that includes prefixes and suffixes
                                column_names <- paste0(prefix,
                                                        c("concept_id",
                                                         "concept_name"
                                                         # ,
                                                         # "domain_id",
                                                         # "vocabulary_id",
                                                         # "concept_class_id",
                                                         # "standard_concept",
                                                         # "concept_code",
                                                         # "valid_start_date",
                                                         # "valid_end_date",
                                                         # "invalid_reason"
                                                         )
                                                       ,
                                                       suffix) %>%
                                                as.list()

                                names(column_names) <-  c("concept_id",
                                                          "concept_name"
                                                          # ,
                                                          # "domain_id",
                                                          # "vocabulary_id",
                                                          # "concept_class_id",
                                                          # "standard_concept",
                                                          # "concept_code",
                                                          # "valid_start_date",
                                                          # "valid_end_date",
                                                          # "invalid_reason"
                                                          )

                                if (!(all(unlist(column_names) %in% colnames(data)))) {

                                        qa <- unlist(column_names)[!(unlist(column_names) %in% colnames(data))]

                                        if (length(qa)) {
                                                stop("missing columns: ", qa)
                                        }

                                }

                                output <-
                                data %>%
                                        tidyr::unite(col = !!into,
                                                     all_of(c(column_names$concept_id,
                                                              column_names$concept_name)),
                                                     sep = " ",
                                                     remove = FALSE)


                                # If All NA concepts are not merged into a strip and returns a single NA
                                output <-
                                    output %>%
                                    dplyr::mutate_at(dplyr::vars(!!into),
                                                     function(x) ifelse(grepl("NA NA",
                                                                              x,
                                                                              ignore.case = FALSE),
                                                                        NA_character_,
                                                                        x))


                                # Normalizing all NA to be able to get a flag for any mis-merged concepts
                                output <-
                                        output %>%
                                        tibble::as_tibble() %>%
                                        dplyr::mutate_all(as.character) %>%
                                        rubix::normalize_all_to_na()

                                # QA NA merges
                                qa <- output %>%
                                        dplyr::filter_at(dplyr::vars(dplyr::all_of(column_names$concept_id)), dplyr::all_vars(!is.na(.))) %>%
                                        dplyr::filter_at(dplyr::vars(!!into), dplyr::all_vars(is.na(.)))

                                if (nrow(qa)) {
                                        flagMergeLabel <<- qa
                                        warning(nrow(qa), ' where concept id is not <NA>, but label is <NA>. See flagMergeLabel object.')
                                }

                                if (remove) {

                                        column_names <- paste0(prefix,
                                                               c("concept_id",
                                                                 "concept_name",
                                                                 "domain_id",
                                                                 "vocabulary_id",
                                                                 "concept_class_id",
                                                                 "standard_concept",
                                                                 "concept_code",
                                                                 "valid_start_date",
                                                                 "valid_end_date",
                                                                 "invalid_reason"
                                                               )
                                                               ,
                                                               suffix) %>%
                                                as.list()

                                        names(column_names) <-  c("concept_id",
                                                                  "concept_name",
                                                                  "domain_id",
                                                                  "vocabulary_id",
                                                                  "concept_class_id",
                                                                  "standard_concept",
                                                                  "concept_code",
                                                                  "valid_start_date",
                                                                  "valid_end_date",
                                                                  "invalid_reason"
                                        )


                                        output <-
                                                output %>%
                                                dplyr::select_at(dplyr::vars(!any_of(c(column_names$concept_id,
                                                                     column_names$concept_name,
                                                                     column_names$domain_id,
                                                                     column_names$vocabulary_id,
                                                                     column_names$concept_class_id,
                                                                     column_names$standard_concept,
                                                                     column_names$concept_code,
                                                                     column_names$valid_start_date,
                                                                     column_names$valid_end_date,
                                                                     column_names$invalid_reason))))


                                }



                                return(output)

            }





#' Merge OMOP Concepts into a Strip
#' @description
#' All the OMOP Vocabulary Concept Table fields other than the date fields are "merged"into a single string, called a "Strip". If the Strip output is `<NA>` while the input concept id is not, a flagMergeStrip object is returned in the Global Environment.
#' @return A tibble with all blank and "NA" normalized to `<NA>` with 1. If present, `valid_start_date` and `valid_end_date` fields are permanently removed, 2. 8 out of the 10 remaining Concept Table fields (concept_id, concept_name, domain_id, vocabulary_id, concept_class_id, standard_concept, concept_code, invalid_reason) are merged into a single column with the provided column name, 3. the concept_id column is renamed to the format of the provided merged column name: {into_}concept_id. The remaining of the 7 Concept Table fields may also be preserved outside of the merge if provided. All other columns present in the input data are returned along with the transformations described.
#' @param data dataframe with the following required fields from the output
#' @param into name of the column that the new combined string will be in
#' @param ... columns other than concept_id that will be removed in tidyr unite but should be preserved in addition to be merged.
#' @param suffix if the omop concept element column names are different from the standard by a suffix, include it so it can point to the correct set of columns
#' @param prefix if the omop concept element column names are prefixed, include it so it can point to the correct set of columns
#' @importFrom tibble as_tibble
#' @noRd

mergeStrip <-
            function(data,
                     into,
                     ...,
                     suffix = NULL,
                     prefix = NULL) {


                                into_id_colname <- paste0(into, "_id")

                                # Enquo output column name
                                into <- dplyr::enquo(into)
                                # Preserve columns
                                preserve_cols <- dplyr::enquos(...)


                                # Generating a list of concept table columns that includes prefixes and suffixes
                                column_names <- paste0(prefix,
                                                        c("concept_id",
                                                         "concept_name",
                                                         "domain_id",
                                                         "vocabulary_id",
                                                         "concept_class_id",
                                                         "standard_concept",
                                                         "concept_code",
                                                         "valid_start_date",
                                                         "valid_end_date",
                                                         "invalid_reason"),
                                                       suffix) %>%
                                                as.list()

                                concept_fields <-  c("concept_id",
                                                          "concept_name",
                                                          "domain_id",
                                                          "vocabulary_id",
                                                          "concept_class_id",
                                                          "standard_concept",
                                                          "concept_code",
                                                          "valid_start_date",
                                                          "valid_end_date",
                                                          "invalid_reason")

                                names(column_names) <- concept_fields


                                if (!(all(unlist(column_names) %in% colnames(data)))) {

                                        stop(sprintf("missing column names: %s", paste(unlist(column_names), collapse = ", ")))

                                }

                                # All other column names
                                other_cols <<- colnames(data)[!(colnames(data) %in% unlist(column_names))]


                                output <-
                                data %>%
                                        dplyr::mutate_at(dplyr::vars(dplyr::all_of(column_names$standard_concept)), function(x) ifelse(is.na(x), "N", x)) %>%
                                        dplyr::mutate_at(dplyr::vars(dplyr::all_of(column_names$standard_concept)), function(x) paste0("[", x, "]")) %>%
                                        dplyr::mutate_at(dplyr::vars(dplyr::all_of(column_names$invalid_reason)), function(x) ifelse(is.na(x), "[V]", paste0("[", x, "]"))) %>%
                                        tidyr::unite(col = vocabulary,
                                                     dplyr::all_of(c(column_names$vocabulary_id,
                                                              column_names$concept_code)),
                                                     sep = " ") %>%
                                        dplyr::mutate_at(dplyr::vars(dplyr::all_of(c(column_names$domain_id,
                                                                       "vocabulary",
                                                                       column_names$concept_class_id))),
                                                         function(x) paste0("[", x, "]")) %>%
                                        #dplyr::select_at(dplyr::vars(!matches("valid.*date"))) %>%
                                        tidyr::unite(col = {{ into }},
                                                     all_of(c(column_names$invalid_reason,
                                                              column_names$standard_concept,
                                                              column_names$concept_id,
                                                              column_names$concept_name,
                                                              "vocabulary",
                                                              column_names$domain_id,
                                                              column_names$concept_class_id)),
                                                     sep = " ",
                                                     remove = FALSE) %>%
                                        dplyr::select(!!into_id_colname := all_of(column_names$concept_id),
                                                      {{ into }})


                                # If All NA concepts are not merged into a strip and returns a single NA
                                output <-
                                    output %>%
                                    dplyr::mutate_at(dplyr::vars({{ into }}),
                                                     function(x) ifelse(grepl("NA NA \\[NA NA\\] \\[NA\\] \\[NA\\]",
                                                                              x,
                                                                              ignore.case = FALSE),
                                                                        NA_character_,
                                                                        x))


                                # Normalizing all NA to be able to get a flag for any mis-merged concepts
                                output <-
                                        output %>%
                                        tibble::as_tibble() %>%
                                        rubix::normalize_all_to_na()

                                # QA NA merges
                                qa <- output %>%
                                        dplyr::filter_at(dplyr::vars(!!into_id_colname), dplyr::all_vars(!is.na(.))) %>%
                                        dplyr::filter_at(dplyr::vars({{ into }}), dplyr::all_vars(is.na(.)))

                                if (nrow(qa)) {
                                        flagMergeStrip <<- qa
                                        warning(nrow(qa), ' where concept id is not <NA>, but merge strip is <NA>. See flagMergeStrip object.')
                                }



                                if (!missing(...)) {
                                        output <-
                                                dplyr::bind_cols(output,
                                                                 data %>%
                                                                         dplyr::select(!!!preserve_cols))



                                }




                                if (length(other_cols)) {

                                        output <-
                                                dplyr::bind_cols(output,
                                                                 data %>%
                                                                     dplyr::select(dplyr::all_of(other_cols)))

                                }


                                return(output)

            }





#' Parse a Concept Label
#' @description Parse a concept Label in the format of "{concept_id} {concept_name}".
#' @noRd





parseLabel <-
        function(data,
                 label_col,
                 remove = FALSE) {


                label_col <- enquo(label_col)


                data %>%
                        tidyr::extract(col = !!label_col,
                                       into = c("concept_id", "concept_name"),
                                       regex = "(^.*?) (.*$)",
                                       remove = remove)

        }




#' @title Separate Concept Strips by Row
#' @description
#' This function separates a merged Concept Strip Column into rows by new line \"\\n\".
#' @param data A data frame.
#' @param ... Columns to separate across multiple rows that are passed to \code{\link[tidyr]{separate_rows}}.
#' @return
#' A longer tibble if the merged Concept Strip Column/s have greater than 1 Concept Strips.
#' @seealso
#'  \code{\link[tidyr]{separate_rows}}
#' @rdname separateConceptStrip
#' @noRd
#' @importFrom tidyr separate_rows

separateConceptStrip <-
        function(data,
                 ...) {

                tidyr::separate_rows(data,
                                     ...,
                                     sep = "(?<=\\])\n(?=\\[A-Z\\])")
        }




#' Convert a Merge Strip to a Label
#' @noRd

stripToLabel <-
        function(data,
                 merge_col,
                 into,
                 remove = FALSE) {

                unmergeStrip(dataframe = data,
                                          concept_col = {{ merge_col }},
                                          remove = remove) %>%
                        makeLabel(into = {{ into }},
                                  remove = remove)
        }




#' Unmerge OMOP Concept Strip
#' @description This function unmerges an OMOP concept strip created by a 'merge' function using the tidyr extract function. If the input is not a tibble, it will be converted into one with the blanks and "NA" values normalized to `<NA>`. A warning is returned in the console if some concepts fail to unmerge into their respective concept table fields, as determined by all the new column fields having a value of `<NA>` with a non-`<NA>` value in the strip_col instance inputed. Errors will be thrown if the data input already contains columns that will collide with the new columns, the names of which defaults to the names of the original concept table fields: concept_id, concept_name, domain_id, vocabulary_id, concept_class_id, standard_concept, concept_code, invalid_reason. Note that the original concept table fields `valid_start_date` and `valid_end_date` are the only concept table fields are not a requirement in the merge and unmerging process.
#' @return a tibble with all blanks, "NA", and <NA> normalized to NA, with unmerged columns with or without the provided prefix and suffix pasted in postions 1 to 8, followed by the strip column if the remove parameter is FALSE, and the remaining fields present in the input.
#' @param data dataframe
#' @param strip_col column that contains the merged concept strip
#' @param remove remove argument passed to the tidyr extract function. If TRUE, removes strip_col in output.
#' @param r_trimws Due to some of the carriage returns in aggregate transformations and other edits in Excel, r_trimws is an argument that if TRUE, trims right whitespace of the freshly unmerged columns for any trailing carriage returns.
#' @importFrom tidyr extract
#' @import dplyr
#' @importFrom stringr str_remove_all
#' @importFrom tibble as_tibble
#' @importFrom rubix normalize_all_to_na
#' @noRd

unmergeStrip <-
    function(data,
             strip_col,
             add_suffix = NULL,
             add_prefix = NULL,
             remove = TRUE,
             r_trimws = TRUE) {

                    strip_col <- dplyr::enquo(strip_col)

                    colOrder <- c("invalid_reason",
                                  "standard_concept",
                                  "concept_id",
                                  "concept_name",
                                  "vocabulary_id",
                                  "concept_code",
                                  "domain_id",
                                  "concept_class_id")

                    new_cols <- paste0(add_prefix,
                                       colOrder,
                                       add_suffix) %>%
                                as.list()

                    names(new_cols) <- colOrder

                    new_cols <- new_cols

                    if (any(unlist(new_cols) %in% colnames(data))) {
                            qa <- unlist(new_cols)[unlist(new_cols) %in% colnames(data)]
                            stop('new column names already present: ', paste(qa, collapse = ", "))
                    }

                    output <-
                    data %>%
                        tidyr::extract(col = !!strip_col,
                                       remove = FALSE,
                                       into = unlist(new_cols),
                                       regex = "(\\[.{1}\\]) (\\[.{1}\\]) ([^ ]*) (.*?) (\\[.*?) (.*?\\]) (\\[.*?\\]) (\\[.*?\\])") %>%
                           tibble::as_tibble() %>%
                            rubix::normalize_all_to_na()

                    output <-
                        output %>%
                                dplyr::mutate_at(dplyr::vars(dplyr::all_of(unlist(new_cols))), stringr::str_remove_all, "^\\[|\\]$") %>%
                                dplyr::mutate_at(dplyr::vars(new_cols$standard_concept, new_cols$invalid_reason), stringr::str_replace_all, "^N$|^V$", NA_character_) %>%
                                dplyr::select(dplyr::all_of(c(new_cols$concept_id,
                                                       new_cols$concept_name,
                                                       new_cols$domain_id,
                                                       new_cols$vocabulary_id,
                                                       new_cols$concept_class_id,
                                                       new_cols$standard_concept,
                                                       new_cols$concept_code,
                                                       new_cols$invalid_reason)),
                                              dplyr::everything())

                    if (r_trimws == TRUE) {

                            output <-
                                output %>%
                                dplyr::mutate_at(dplyr::vars(dplyr::all_of(c(new_cols$concept_id,
                                                               new_cols$concept_name,
                                                               new_cols$domain_id,
                                                               new_cols$vocabulary_id,
                                                               new_cols$concept_class_id,
                                                               new_cols$standard_concept,
                                                               new_cols$concept_code,
                                                               new_cols$invalid_reason))),
                                                 base::trimws)

                    }

                    qa <-
                        output %>%
                        dplyr::filter_at(dplyr::vars(c(new_cols$concept_id,
                                                new_cols$concept_name,
                                                new_cols$domain_id,
                                                new_cols$vocabulary_id,
                                                new_cols$concept_class_id,
                                                new_cols$standard_concept,
                                                new_cols$concept_code,
                                                new_cols$invalid_reason)),
                                         dplyr::all_vars(is.na(.))) %>%
                        dplyr::filter_at(dplyr::vars(!!strip_col),
                                         dplyr::all_vars(!is.na(.)))

                    if (nrow(qa) > 0) {


                            flagUnmergeStrip <<- qa

                            warning('Not all concepts unmerged: ', nrow(qa), '. See flagUnmergeStrip object.')


                    }

                    if (remove) {

                        output <-
                            output %>%
                            dplyr::select(-!!strip_col)

                    }

                    return(output)

    }







#' QA to make sure the concept_id exists in OMOP
#' @noRd


conceptIdExists <-
    function(concept_id,
             schema) {


            .Deprecated("concept_id_exists")

                    x <- queryConceptId(concept_ids = concept_id,
                                        schema = schema,
                                        override_cache = TRUE)

                    if (nrow(x) > 0) {

                            TRUE

                    } else {

                           FALSE
                    }

    }





#' Returns all possible OMOP domains in Athena
#' @return dataframe of unique `domain_id` variables from the `concept` table
#' @noRd

return_omop_vocabs <-
        function() {
                resultset <- query_athena("SELECT DISTINCT vocabulary_id FROM public.concept;")
                return(resultset)

        }



#' @title
#' Get all OMOP Concept Classes
#' @return
#' A data frame of unique `domain_id`, `vocabulary_id`, and `concept_class_id` combinations from the Concept Table.
#' @seealso
#'  \code{\link[pg13]{buildQuery}}
#' @rdname getConceptClasses
#' @noRd
#' @importFrom pg13 buildQuery

getConceptClasses <-
        function(schema = NULL,
                 verbose = FALSE,
                 cache_resultset = TRUE,
                 override_cache = FALSE,
                 conn = NULL,
                 render_sql = FALSE,
                 sleepTime = 1,
                 ...) {


                sql_statement <-
                        pg13::build_query(fields = c("domain_id", "vocabulary_id", "concept_class_id"),
                                         distinct = TRUE,
                                         schema = schema,
                                         tableName = "concept")

                queryAthena(sql_statement = sql_statement,
                            verbose = verbose,
                            cache_resultset = cache_resultset,
                            override_cache = override_cache,
                            conn = conn,
                            render_sql = render_sql,
                            sleepTime = sleepTime,
                            ...)
        }


#' @title
#' Get all OMOP Domain
#' @return
#' A data frame of unique `domain_id`, `vocabulary_id`, and `concept_class_id` combinations from the Concept Table.
#' @seealso
#'  \code{\link[pg13]{buildQuery}}
#' @rdname getConceptClasses
#' @noRd
#' @importFrom pg13 buildQuery

getDomain <-
        function(schema = NULL,
                 verbose = FALSE,
                 cache_resultset = TRUE,
                 override_cache = FALSE,
                 conn = NULL,
                 render_sql = FALSE,
                 sleepTime = 1,
                 ...) {


                sql_statement <-
                        pg13::build_query(fields = c("domain_id"),
                                         distinct = TRUE,
                                         schema = schema,
                                         tableName = "concept")

                queryAthena(sql_statement = sql_statement,
                            verbose = verbose,
                            cache_resultset = cache_resultset,
                            override_cache = override_cache,
                            conn = conn,
                            render_sql = render_sql,
                            sleepTime = sleepTime,
                            ...)
        }






#' Extract HemOnc Regimens by Component String Match
#' @description This function uses the grepl function on the HemOnc Concept Names and naming patterns to return HemOnc Regimens based on a single Component and total number of Components in the source regimen.
#' @param component_count If NULL or the component_count is larger than the maximum number of components possible for all Regimens with a positive string match to the `component` parameter, the unfiltered result of the initial query for `component` is returned.
#' @importFrom rubix filter_at_grepl
#' @importFrom rubix arrange_by_nchar
#' @noRd

extractHemOncRegimens <-
    function(component,
             component_count = NULL,
             omop = FALSE,
             omop_schema = "omop_vocabulary") {


        .Deprecated()

        output <-
                query_phrase(component,
                             type = "like",
                             where_col = "vocabulary_id",
                             where_col_in = "HemOnc",
                             omop = omop,
                             omop_schema = omop_schema)

        output <-
        output %>%
            rubix::filter_at_grepl(concept_name,
                                   grepl_phrase = ", | and | monotherapy") %>%
            rubix::arrange_by_nchar(nchar_col = concept_name)

        if (is.null(component_count)) {
                return(output)
        } else if (component_count == 1) {

            output <-
                output %>%
                rubix::filter_at_grepl(concept_name,
                                       grepl_phrase = " monotherapy") %>%
                rubix::arrange_by_nchar(nchar_col = concept_name)

            return(output)

        } else if (component_count == 2) {

                output %>%
                    rubix::filter_at_grepl(concept_name,
                                           grepl_phrase = " and ") %>%
                rubix::arrange_by_nchar(nchar_col = concept_name)
        } else if (component_count == 3) {


                output %>%
                        dplyr::mutate(comma_count = centipede::nchar_comma(concept_name)) %>%
                        dplyr::filter(comma_count == 2) %>%
                rubix::arrange_by_nchar(nchar_col = concept_name)

        } else if (component_count == 4) {

                output %>%
                    dplyr::mutate(comma_count = centipede::nchar_comma(concept_name)) %>%
                    dplyr::filter(comma_count == 3) %>%
                    rubix::arrange_by_nchar(nchar_col = concept_name)

        } else {
            max <-  1 + (output %>%
                        dplyr::transmute(comma_count = centipede::nchar_comma(concept_name)) %>%
                        unlist() %>%
                        max(na.rm = TRUE))
            warning('"component_count" max is: ', max,  ' returning unfiltered output')
            return(output)

        }
    }


#' Normalize To HemOnc Components
#' @description This function takes a mixture of HemOnc Regimen and HemOnc Component Concepts and returns all the unique HemOnc Components associated with the input combination.
#' @param hemonc_concept_ids HemOnc Vocabulary Concept Ids of either Regimen or Component concept classes.
#' @import rubix
#' @import dplyr
#' @noRd



normalizeToHemOncComponents <-
    function(hemonc_concept_ids,
             schema = NULL) {


        .Deprecated()

        # If any of the concept_ids are regimens, to get their antineoplastic components
        input_concept <- query_concept_id(hemonc_concept_ids)

        qa <- input_concept %>%
            rubix::filter_for(filter_col = concept_class_id,
                              inclusion_vector = c("Regimen",
                                                   "Component"),
                              invert = TRUE)

        if (nrow(qa)) {
            qaNormalizeToHemOncComponents <<- qa
            stop('input concept ids are not Regimen or Components. See qaNormalizeToHemOncComponents for more details.')
        }

        input_regimens <- input_concept %>%
            dplyr::filter(concept_class_id == "Regimen")

        input_components <- input_concept %>%
            dplyr::filter(concept_class_id == "Component")


        if (nrow(input_regimens)) {


            component_concept_ids <-
                c(input_components$concept_id,
                  queryHemOncRegToAntineo(regimen_concept_ids = input_regimens$concept_id,
                                          schema = schema) %>%
                      dplyr::select(has_antineoplastic_concept_id) %>%
                      unlist() %>%
                      unname())


        } else {

            component_concept_ids <- input_components$concept_id

        }

        return(unique(component_concept_ids))
    }





#' @noRd

ids_to_integer <-
        function(data) {

                .Deprecated()

                data %>%
                        dplyr::mutate_at(dplyr::vars(contains("concept_id")),
                                         as.integer)

        }



lowLevelQuery <-
        function (conn,
                  conn_fun,
                  sql_statement,
                  verbose = TRUE,
                  render_sql = TRUE,
                  render_only = FALSE,
                  ...)

        {

                .Deprecated(new = "query",
                            package = "pg13")
                if (render_only) {
                        typewrite_sql(sql_statement = sql_statement)
                }
                else {
                        if (!missing(conn_fun)) {
                                conn <- eval(rlang::parse_expr(conn_fun))
                                on.exit(dc(conn = conn))
                        }
                        cli::cat_rule("Checks")
                        check_conn(conn = conn)
                        if (render_sql) {
                                typewrite_sql(sql_statement = sql_statement)
                        }

                        if (verbose) {
                                typewrite_activity("Querying...")
                        }
                        resultset <- DatabaseConnector::dbGetQuery(conn, statement = sql_statement,
                                                                   ...)
                        if (verbose) {
                                typewrite_activity("Querying...complete")
                        }
                        resultset
                }
        }





#' @title Query the Athena Postgres Database
#' @description
#' By default, this function queries a local database named "Athena". If a connection object is passed into the function, the database of the connection object is queried instead. The caching feature is only available when using the built-in connection to Athena.
#'
#' @param sql_statement         SQL query
#' @param cache_only            Loads from the cache and does not query the database. A NULL object is returned if a resultset was not cached.
#' @param skip_cache            Skip the caching altogether and directly query the database.
#' @param override_cache        If TRUE, the cache will not be loaded and will be overwritten by a new query. For override_cache to take effect, skip_cache should be FALSE.
#' @param conn                  Connection object. If provided, diverts queries to the connection instead of the local Athena instance without caching features.
#' @param render_sql            If TRUE, the SQL will be printed back in the console prior to execution. Default: FALSE
#' @param verbose               If TRUE, prints loading and querying operations messages to the console. Default: FALSE
#' @param sleepTime             Argument for `Sys.sleep()` in between queries to allow for halting function execution, especially in cases where other chariot functions are executing multiple queries in succession and require cancellation.
#' @param cache_resultset       (deprecated) If TRUE, the resultset from the query will first be loaded from the cache. The query will be executed if a cached resultset is not retrieved for this particular query, after which the resultset will be cached. If FALSE, Athena or conn will be directly queried without any caching operations.
#'
#' @return
#' A tibble
#'
#' @seealso
#'  \code{\link[secretary]{typewrite_bold}},\code{\link[secretary]{typewrite}}
#'  \code{\link[stringr]{str_replace}},\code{\link[stringr]{str_remove}}
#'  \code{\link[pg13]{query}},\code{\link[pg13]{cacheQuery}},\code{\link[pg13]{loadCachedQuery}}
#'  \code{\link[tibble]{as_tibble}}
#' @rdname queryAthena
#' @export
#' @importFrom secretary typewrite_bold typewrite
#' @importFrom stringr str_replace_all str_remove_all
#' @importFrom tibble as_tibble


executeAthena <-
        function(sql_statement,
                 conn,
                 conn_fun = "connectAthena()",
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 cache_resultset = TRUE,
                 render_sql = FALSE,
                 verbose = FALSE,
                 sleepTime = 1) {


                .Deprecated(new = "query")

                if (missing(conn)) {

                        conn <- eval(expr = rlang::parse_expr(x = conn_fun))
                        on.exit(expr = dcAthena(conn = conn,
                                                verbose = verbose),
                                add = TRUE,
                                after = TRUE)

                }

                check_conn(conn = conn)


                if (skip_cache) {

                        if (verbose) {
                                secretary::typewrite("Skipping cache")
                        }

                        resultset <-  pg13::query(conn = conn,
                                                  sql_statement = sql_statement,
                                                  verbose = verbose,
                                                  render_sql = render_sql,
                                                  render_only = render_only)


                } else {

                        if (override_cache) {

                                if (verbose) {
                                        secretary::typewrite("Overriding cache")
                                }

                                resultset <-  pg13::query(conn = conn,
                                                          sql_statement = sql_statement,
                                                          verbose = verbose,
                                                          render_sql = render_sql,
                                                          render_only = render_only)


                                lowLevelCache(data = resultset,
                                              query = sql_statement)


                        } else {

                                if (verbose) {
                                        secretary::typewrite("Loading Cache")
                                }


                                resultset <- lowLevelLoadCache(query = sql_statement)

                                if (!cache_only) {

                                        if (is.null(resultset)) {


                                                if (verbose) {
                                                        secretary::typewrite("Cache was NULL, querying Athena")
                                                }

                                                Sys.sleep(time = sleepTime)
                                                resultset <-  pg13::query(conn = conn,
                                                                          sql_statement = sql_statement,
                                                                          verbose = verbose,
                                                                          render_sql = render_sql,
                                                                          render_only = render_only)


                                                lowLevelCache(data = resultset,
                                                              query = sql_statement)

                                        }

                                } else {

                                        if (verbose) {

                                                secretary::typewrite_bold("Loaded resultset from cache", line_number = 0)

                                        }
                                }

                        }
                }

                tibble::as_tibble(resultset)

        }


#' Get Connection Database Name
#' @description
#' This is to make sure that the cache path directory has the same name as the database to prevent collisions between multiple OMOP Vocabulary sources used simulataneously.
#' @export

get_conn_db <-
        function(conn) {

                .Deprecated(package = "pg13")

                conn@jConnection$getCatalog()
        }


makeSelectFields <-
        function(shortcut = NULL,
                 schema,
                 tableName,
                 prefix = NULL,
                 suffix = NULL,
                 conn) {

                if (is.null(prefix) && is.null(suffix)) {

                        stop("At least one prefix or one suffix must be supplied.")

                }

                if (!is.null(shortcut)) {
                        shortcut <- sprintf("%s.", shortcut)
                }

                fields <- pg13::lsFields(conn = conn,
                                         schema = schema,
                                         tableName = tableName)


                paste0(shortcut, fields, " AS ", prefix, fields, suffix) %>%
                        paste(collapse = ",\n") %>%
                        cat()

        }


