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
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[SqlRender]{render}}
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{select}},\code{\link[dplyr]{reexports}},\code{\link[dplyr]{distinct}},\code{\link[dplyr]{bind}},\code{\link[dplyr]{mutate_all}},\code{\link[dplyr]{group_by}},\code{\link[dplyr]{summarise_all}},\code{\link[dplyr]{mutate-joins}}
#'  \code{\link[tidyr]{unite}},\code{\link[tidyr]{pivot_longer}}
#'  \code{\link[colorspace]{rainbow_hcl}}
#'  \code{\link[collapsibleTree]{collapsibleTreeNetwork}}
#' @rdname plot_classification
#' @noRd
#' @importFrom tibble tibble
#' @importFrom SqlRender render
#' @importFrom dplyr mutate select everything distinct bind_rows mutate_all group_by summarize_at ungroup left_join
#' @importFrom tidyr unite pivot_longer
#' @importFrom colorspace terrain_hcl
#' @importFrom collapsibleTree collapsibleTreeNetwork

plot_classification <-
  function(vocabulary_id,
           domain_id,
           conn,
           vocab_schema,
           range = 1:10,
           color_by = "vocabulary_id",
           terminal_vocabulary_id,
           verbose = TRUE,
           render_sql = TRUE) {
    child <- paste0(vocabulary_id, " ", domain_id)
    root <-
      tibble::tibble(
        parent = NA_character_,
        child = child
      )

    vocabulary_id <- paste0("'", vocabulary_id, "'")
    terminal_vocabulary_id <- paste0("'", terminal_vocabulary_id, "'")
    domain_id <- paste0("'", domain_id, "'")


    level_1 <-
      queryAthena(
        sql_statement =
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
                                    c.vocabulary_id IN (@vocabulary_id)
                                    AND c.standard_concept = 'C'
                                    AND c.invalid_reason IS NULL
                                    AND c2.invalid_reason IS NULL
                                    AND c2.standard_concept = 'C'
                                    AND c2.vocabulary_id IN (@vocabulary_id)
                                    AND c.domain_id = @domain_id
                                    AND c2.domain_id = @domain_id
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
            domain_id = domain_id
          ),
        conn = conn,
        render_sql = TRUE
      )

    stopifnot(nrow(level_1) > 0)

    level_1 <-
      level_1 %>%
      dplyr::mutate(parent = child) %>%
      tidyr::unite(
        col = child,
        concept_id,
        concept_name,
        sep = " ",
        na.rm = TRUE,
        remove = FALSE
      ) %>%
      dplyr::select(
        parent,
        child,
        dplyr::everything()
      )

    range_output <- list()
    range_output[[1]] <- level_1


    for (i in 2:max(range)) {
      new_parents <-
        range_output[[i - 1]] %>%
        dplyr::select(concept_id) %>%
        dplyr::distinct() %>%
        unlist() %>%
        as.integer()

      level_n <-
        queryAthena(
          sql_statement =
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
                                        c.vocabulary_id IN (@vocabulary_id)
                                        AND c.standard_concept = 'C'
                                        AND c.invalid_reason IS NULL
                                        AND c2.invalid_reason IS NULL
                                        AND c2.standard_concept = 'C'
                                        AND c2.vocabulary_id IN (@vocabulary_id)
                                        AND c.domain_id = @domain_id
                                        AND c2.domain_id = @domain_id
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
              new_parents = new_parents
            ),
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


    terminal_class <- range_output[[length(range_output)]] %>%
      dplyr::select(concept_id) %>%
      dplyr::distinct() %>%
      unlist() %>%
      unique()

    terminal_class_concepts <-
      queryAthena(
        sql_statement =
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
                                        c.vocabulary_id IN (@vocabulary_id)
                                        AND c.standard_concept = 'C'
                                        AND c.invalid_reason IS NULL
                                        AND c2.invalid_reason IS NULL
                                        AND c2.standard_concept <> 'C'
                                        AND c2.vocabulary_id IN (@vocabulary_id)
                                        AND c.domain_id = @domain_id
                                        AND c2.domain_id = @domain_id
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
            vocabulary_id = terminal_vocabulary_id,
            domain_id = domain_id,
            new_parents = terminal_class
          ),
        conn = conn,
        verbose = verbose,
        render_sql = render_sql
      )




    df <- dplyr::bind_rows(
      root,
      range_output,
      terminal_class_concepts
    )

    tooltip <-
      df %>%
      dplyr::mutate_all(as.character) %>%
      tidyr::pivot_longer(
        cols = !c(parent, child),
        names_to = "attribute",
        values_to = "attribute_value",
        values_drop_na = TRUE
      ) %>%
      tidyr::unite(
        col = tooltip,
        attribute,
        attribute_value,
        sep = ": ",
        remove = TRUE,
        na.rm = TRUE
      ) %>%
      dplyr::distinct() %>%
      dplyr::group_by(child) %>%
      dplyr::summarize_at(vars(tooltip), ~ paste(., collapse = "<br>")) %>%
      dplyr::ungroup() %>%
      dplyr::distinct()

    color <- unlist(df[, color_by])
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

    collapsibleTree::collapsibleTreeNetwork(
      df = df,
      tooltipHtml = "tooltip",
      fill = "color"
    )
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
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[SqlRender]{render}}
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{select}},\code{\link[dplyr]{reexports}},\code{\link[dplyr]{distinct}},\code{\link[dplyr]{bind}},\code{\link[dplyr]{mutate_all}},\code{\link[dplyr]{group_by}},\code{\link[dplyr]{summarise_all}},\code{\link[dplyr]{mutate-joins}}
#'  \code{\link[tidyr]{unite}},\code{\link[tidyr]{pivot_longer}}
#'  \code{\link[colorspace]{rainbow_hcl}}
#'  \code{\link[collapsibleTree]{collapsibleTreeNetwork}}
#' @rdname plot_concept_classification
#' @noRd
#' @importFrom tibble tibble
#' @importFrom SqlRender render
#' @importFrom dplyr mutate select everything distinct bind_rows mutate_all group_by summarize_at ungroup left_join
#' @importFrom tidyr unite pivot_longer
#' @importFrom colorspace terrain_hcl
#' @importFrom collapsibleTree collapsibleTreeNetwork

plot_concept_classification <-
  function(vocabulary_id,
           domain_id,
           conn,
           vocab_schema,
           range = 1:10,
           color_by = "vocabulary_id",
           terminal_vocabulary_id,
           render_sql = TRUE) {
    child <- paste0(vocabulary_id, " ", domain_id)
    root <-
      tibble::tibble(
        parent = NA_character_,
        child = child
      )

    vocabulary_id <- paste0("'", vocabulary_id, "'")
    terminal_vocabulary_id <- paste0("'", terminal_vocabulary_id, "'")
    domain_id <- paste0("'", domain_id, "'")


    level_1 <-
      queryAthena(
        sql_statement =
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
                                    c.vocabulary_id IN (@vocabulary_id)
                                    AND c.standard_concept = 'C'
                                    AND c.invalid_reason IS NULL
                                    AND c2.invalid_reason IS NULL
                                    AND c2.standard_concept = 'C'
                                    AND c2.vocabulary_id IN (@vocabulary_id)
                                    AND c.domain_id = @domain_id
                                    AND c2.domain_id = @domain_id
                                    AND ca.ancestor_concept_id <> ca.descendant_concept_id
                                    AND ca.min_levels_of_separation = 1 AND ca.max_levels_of_separation = 1
                                )

                            SELECT DISTINCT c.*
                                FROM ancestry a
                            LEFT JOIN @vocab_schema.concept c
                            ON c.concept_id = a.ancestor_concept_id
                            WHERE a.ancestor_concept_id NOT IN (
                                SELECT a2.descendant_concept_id
                                FROM ancestry a2
                            )
                            ;",
            vocab_schema = vocab_schema,
            vocabulary_id = vocabulary_id,
            domain_id = domain_id
          ),
        conn = conn,
        render_sql = TRUE
      )

    level_1 <-
      level_1 %>%
      dplyr::mutate(parent = child) %>%
      tidyr::unite(
        col = child,
        concept_id,
        concept_name,
        sep = " ",
        na.rm = TRUE,
        remove = FALSE
      ) %>%
      dplyr::select(
        parent,
        child,
        dplyr::everything()
      )

    range_output <- list()
    range_output[[1]] <- level_1

    proceed <- TRUE
    for (i in 2:max(range)) {
      if (proceed) {
        new_parents <-
          range_output[[i - 1]] %>%
          dplyr::select(concept_id) %>%
          dplyr::distinct() %>%
          unlist() %>%
          as.integer()

        level_n <-
          queryAthena(
            sql_statement =
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
                                        c.vocabulary_id IN (@vocabulary_id)
                                        AND c.standard_concept = 'C'
                                        AND c.invalid_reason IS NULL
                                        AND c2.invalid_reason IS NULL
                                        AND c2.standard_concept = 'C'
                                        AND c2.vocabulary_id IN (@vocabulary_id)
                                        AND c.domain_id = @domain_id
                                        AND c2.domain_id = @domain_id
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
                new_parents = new_parents
              ),
            conn = conn,
            render_sql = TRUE
          )

        if (nrow(level_n) == 0) {
          proceed <- FALSE
        } else {
          range_output[[i]] <- level_n
        }
      }
    }

    terminal_class <- range_output[[length(range_output)]] %>%
      dplyr::select(concept_id) %>%
      dplyr::distinct() %>%
      unlist() %>%
      unique()

    terminal_class_concepts <-
      queryAthena(
        sql_statement =
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
                                        c.vocabulary_id IN (@vocabulary_id)
                                        AND c.standard_concept = 'C'
                                        AND c.invalid_reason IS NULL
                                        AND c2.invalid_reason IS NULL
                                        AND c2.standard_concept <> 'C'
                                        AND c2.vocabulary_id IN (@vocabulary_id)
                                        AND c.domain_id = @domain_id
                                        AND c2.domain_id = @domain_id
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
            vocabulary_id = terminal_vocabulary_id,
            domain_id = domain_id,
            new_parents = terminal_class
          ),
        conn = conn,
        render_sql = TRUE
      )




    df <- dplyr::bind_rows(
      root,
      range_output,
      terminal_class_concepts
    )

    tooltip <-
      df %>%
      dplyr::mutate_all(as.character) %>%
      tidyr::pivot_longer(
        cols = !c(parent, child),
        names_to = "attribute",
        values_to = "attribute_value",
        values_drop_na = TRUE
      ) %>%
      tidyr::unite(
        col = tooltip,
        attribute,
        attribute_value,
        sep = ": ",
        remove = TRUE,
        na.rm = TRUE
      ) %>%
      dplyr::distinct() %>%
      dplyr::group_by(child) %>%
      dplyr::summarize_at(vars(tooltip), ~ paste(., collapse = "<br>")) %>%
      dplyr::ungroup() %>%
      dplyr::distinct()

    color <- unlist(df[, color_by])
    color[is.na(color)] <- "NA"
    df$color <- factor(color)
    levels(df$color) <- colorspace::terrain_hcl(n = length(levels(df$color)))
    df$color <- as.character(df$color)

    df <-
      df %>%
      dplyr::select(parent, child, color) %>%
      dplyr::left_join(tooltip) %>%
      dplyr::distinct()

    collapsibleTree::collapsibleTreeNetwork(
      df = df,
      tooltipHtml = "tooltip",
      fill = "color"
    )
  }

#' @details invalid_reason is not an argument because it is already filtered out when making the concept_parent table


plot_is_a <-
  function(vocab_schema,
           writeSchema,
           conceptParentSchema,
           vocabulary_id,
           concept_class_id,
           domain_id,
           standard_concept,
           color_col,
           generations = 2:5,
           conn = NULL,
           render_sql = TRUE,
           verbose = FALSE,
           sleepTime = 1) {



    # vocab_schema <- "omop_vocabulary"
    # writeSchema <- "patelm9"
    # conceptParentSchema <- "patelm9"
    # vocabulary_id <- "HemOnc"
    # domain_id <- "Drug"
    # color_col <- domain_id
    # generations <- 2:5
    # conn <- fantasia::qOMOP()
    # render_sql <- TRUE
    # verbose <- FALSE
    # sleepTime <- 1


    concept_filters <- generate_concept_filters(
      vocab_schema = vocab_schema,
      vocabulary_id = vocabulary_id,
      domain_id = domain_id,
      concept_class_id = concept_class_id,
      standard_concept = standard_concept
    )

    if (is.null(concept_filters)) {
      stop("`vocabulary_id`, `concept_class_id`, `domain_id`, and/or `standard_concept` required")
    }

    output <-
      queryAthena(
        SqlRender::render(
          "
                                        WITH target_cids AS (
                                                SELECT DISTINCT
                                                            concept_id
                                                FROM @vocab_schema.concept
                                                WHERE @concept_filters
                                        )

                                        SELECT
                                            cp.parent_concept_id,
                                            cp.child_concept_id
                                        FROM @conceptParentSchema.concept_parent cp
                                        INNER JOIN target_cids t
                                        ON t.concept_id = cp.parent_concept_id
                                        UNION
                                        SELECT
                                             cp.parent_concept_id,
                                             cp.child_concept_id
                                        FROM @conceptParentSchema.concept_parent cp
                                        INNER JOIN target_cids t
                                        ON t.concept_id = cp.child_concept_id
                                        ;",
          vocab_schema = vocab_schema,
          conceptParentSchema = conceptParentSchema,
          concept_filters = concept_filters
        ),
        conn = conn,
        skip_cache = TRUE,
        render_sql = render_sql,
        verbose = verbose,
        sleepTime = sleepTime
      )

    # Creating the top bud from the topmost concepts
    output_b <-
      output %>%
      dplyr::full_join(output, by = c("parent_concept_id" = "child_concept_id")) %>%
      dplyr::rename(grandparent_concept_id = parent_concept_id.y) %>%
      dplyr::filter(is.na(grandparent_concept_id)) %>%
      dplyr::select(
        parent_concept_id = grandparent_concept_id,
        child_concept_id = parent_concept_id
      ) %>%
      dplyr::distinct() %>%
      dplyr::mutate(new_parent_concept_id = 0) %>%
      dplyr::mutate(parent_concept_id = dplyr::coalesce(parent_concept_id, new_parent_concept_id)) %>%
      dplyr::select(-new_parent_concept_id)

    output_c <-
      tibble::tibble(
        parent_concept_id = NA,
        child_concept_id = 0
      )


    output2 <-
      dplyr::bind_rows(
        output_c,
        output_b,
        output
      ) %>%
      dplyr::mutate(child_concept_id = as.integer(child_concept_id))


    output3b <-
      leftJoinConceptId(output2,
        column = "child_concept_id",
        writeSchema = writeSchema,
        athena_schema = vocab_schema,
        conn = conn
      )

    output4 <-
      output2 %>%
      dplyr::left_join(output3b, by = c("parent_concept_id", "child_concept_id")) %>%
      dplyr::distinct()

    output5 <-
      output4 %>%
      dplyr::mutate(
        concept_name = ifelse(concept_id == 0, "Bud", concept_name),
        domain_id = ifelse(concept_id == 0, NA_character_, domain_id),
        concept_class_id = ifelse(concept_id == 0, NA_character_, concept_class_id)
      )

    tooltip <-
      output5 %>%
      # Converting all columns to be pivoted to character otherwise cannot be combined
      dplyr::mutate_at(vars(!c(parent_concept_id, child_concept_id)), as.character) %>%
      tidyr::pivot_longer(
        cols = !c(parent_concept_id, child_concept_id),
        names_to = "tooltip",
        values_to = "tooltip_value",
        values_drop_na = TRUE
      ) %>%
      dplyr::distinct() %>%
      tidyr::unite(
        col = tooltip,
        tooltip,
        tooltip_value,
        sep = ": ",
        remove = TRUE,
        na.rm = TRUE
      ) %>%
      dplyr::group_by(child_concept_id) %>%
      dplyr::summarize_at(vars(tooltip), ~ paste(., collapse = "<br>")) %>%
      dplyr::ungroup()

    output6 <-
      output5 %>%
      dplyr::left_join(tooltip) %>%
      dplyr::distinct() %>%
      dplyr::mutate_all(as.character)


    output7 <-
      output6 %>%
      tidyr::unite(
        col = child,
        concept_id,
        concept_name,
        sep = " ",
        remove = FALSE,
        na.rm = TRUE
      )

    output7b <-
      output7 %>%
      dplyr::mutate(color := {{ color_col }}) %>%
      dplyr::mutate(color = factor(color))

    levels(output7b$color) <- colorspace::rainbow_hcl(n = length(levels(output7b$color)))



    output8 <-
      output7b %>%
      dplyr::left_join(output7 %>%
        dplyr::select(
          parent_concept_id = child_concept_id,
          parent = child
        ) %>%
        dplyr::distinct(),
      by = "parent_concept_id"
      ) %>%
      dplyr::distinct()


    output8 <-
      output8 %>%
      dplyr::select(parent, child, parent_concept_id, child_concept_id, tooltip, color) %>%
      dplyr::mutate(
        parent_concept_id = as.integer(parent_concept_id),
        child_concept_id = as.integer(child_concept_id)
      )


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
          levels[[i - 1]] %>%
          dplyr::select(
            parent = child,
            parent_concept_id = child_concept_id
          ) %>%
          dplyr::inner_join(output8, by = c("parent", "parent_concept_id")) %>%
          dplyr::select(-child) %>%
          leftJoinConceptId(column = "child_concept_id", writeSchema = writeSchema, athena_schema = vocab_schema, conn = conn) %>%
          tidyr::unite(
            col = child,
            child_concept_id,
            concept_name,
            sep = " ",
            remove = FALSE,
            na.rm = TRUE
          ) %>%
          dplyr::select(
            parent_concept_id,
            child_concept_id,
            parent,
            child,
            tooltip,
            color
          ) %>%
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
              NA
            )) %>%
            tidyr::unite(
              col = child2,
              child,
              label_append,
              sep = " ",
              remove = TRUE,
              na.rm = TRUE
            ) %>%
            dplyr::distinct() %>%
            dplyr::rename(child = child2) %>%
            dplyr::arrange(parent_concept_id, child_concept_id)

          levels[[i]] <- x2
          names(levels)[i] <- paste0("level_", i - 1)
        }
      }
    }

    # Add 1 because root has a slot in the list
    starting_index <- grep(pattern = min(generations), x = names(levels))

    if (length(starting_index) == 0) {
      starting_index <- 3
    }

    ending_index <- grep(pattern = max(generations), x = names(levels))

    if (length(ending_index) == 0) {
      ending_index <- length(levels)
    }

    updated_levels <-
      list(
        purrr::pluck(levels, "root"),
        purrr::pluck(levels, "level_1"),
        purrr::pluck(levels, starting_index),
        purrr::pluck(levels, ending_index)
      )

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
      error = function(e) {
        return(updated_levels)
      }
    )
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
      queryAthena(
        sql_statement =
          SqlRender::render(
            "
                                SELECT *
                                FROM @vocab_schema.concept_ancestor ca
                                WHERE ca.descendant_concept_id IN (@concept_ids)
                                    AND ca.max_levels_of_separation = 1
                                ",
            vocab_schema = vocab_schema,
            concept_ids = concept_ids
          ),
        conn = conn
      )


    for (i in 2:ancestor_generations) {
      new_descendant_concept_ids <- output_a[[i - 1]]$ancestor_concept_id
      output_a[[i]] <-
        queryAthena(
          sql_statement =
            SqlRender::render(
              "
                                SELECT *
                                FROM @vocab_schema.concept_ancestor ca
                                WHERE ca.descendant_concept_id IN (@new_descendant_concept_ids)
                                    AND ca.max_levels_of_separation = 1
                                ",
              vocab_schema = vocab_schema,
              new_descendant_concept_ids = new_descendant_concept_ids
            ),
          conn = conn
        )
    }

    output_a <- rev(output_a)

    output_d <- list()
    output_d[[1]] <-
      queryAthena(
        sql_statement =
          SqlRender::render(
            "
                                SELECT *
                                FROM @vocab_schema.concept_ancestor ca
                                WHERE ca.ancestor_concept_id IN (@concept_ids)
                                    AND ca.max_levels_of_separation = 1
                                ",
            vocab_schema = vocab_schema,
            concept_ids = concept_ids
          ),
        conn = conn
      )


    for (i in 2:descendant_generations) {
      new_ancestor_concept_ids <- output_a[[i - 1]]$descendant_concept_id
      output_d[[i]] <-
        queryAthena(
          sql_statement =
            SqlRender::render(
              "
                                    SELECT *
                                    FROM @vocab_schema.concept_ancestor ca
                                    WHERE ca.ancestor_concept_id IN (@new_ancestor_concept_ids)
                                        AND ca.max_levels_of_separation = 1
                                    ",
              vocab_schema = vocab_schema,
              new_ancestor_concept_ids = new_ancestor_concept_ids
            ),
          conn = conn
        )
    }

    output <-
      c(
        output_a,
        output_d
      )

    # level 1
    level_1 <-
      tibble::tibble(descendant_concept_id = unique(output[[1]]$ancestor_concept_id)) %>%
      dplyr::mutate(ancestor_concept_id = 00000) %>%
      dplyr::select(
        ancestor_concept_id,
        descendant_concept_id
      )

    # root
    root <-
      tibble::tibble(
        ancestor_concept_id = NA,
        descendant_concept_id = 00000
      )


    final <-
      dplyr::bind_rows(
        root,
        level_1,
        output
      ) %>%
      dplyr::select(
        parent_concept_id = ancestor_concept_id,
        child_concept_id = descendant_concept_id
      ) %>%
      dplyr::distinct() %>%
      tibble::rowid_to_column() %>%
      tidyr::pivot_longer(
        cols = c(parent_concept_id, child_concept_id),
        names_to = "relative_type",
        values_to = "relative_concept_id"
      ) %>%
      leftJoinConceptId(
        column = "relative_concept_id",
        writeSchema = writeSchema
      )


    final2 <-
      final %>%
      dplyr::mutate(color := {{ color_col }}) %>%
      dplyr::mutate(color = factor(color))

    levels(final2$color) <- colorspace::terrain_hcl(n = length(levels(final2$color)))

    final3 <-
      final2 %>%
      dplyr::mutate_at(vars(!c(rowid, relative_type, relative_concept_id, color)), as.character) %>%
      tidyr::pivot_longer(
        cols = !c(rowid, relative_type, relative_concept_id, color),
        names_to = "concept_field",
        values_to = "concept_field_values",
        values_drop_na = TRUE
      ) %>%
      dplyr::distinct()


    final4 <-
      dplyr::left_join(
        final3,
        final3 %>%
          tidyr::unite(
            col = tooltip,
            concept_field,
            concept_field_values,
            sep = ": ",
            remove = TRUE,
            na.rm = TRUE
          ) %>%
          dplyr::group_by(rowid, relative_type, relative_concept_id) %>%
          dplyr::summarize_at(vars(tooltip), ~ paste(unique(.), collapse = "<br>")) %>%
          dplyr::ungroup()
      ) %>%
      dplyr::select(-concept_field, -concept_field_values) %>%
      dplyr::distinct() %>%
      tidyr::pivot_wider(
        id_col = c(rowid, color),
        names_from = relative_type,
        values_from = c(relative_concept_id)
      )


    return(final4)


    return(final)


    resultset2 <-
      dplyr::left_join(
        resultset,
        resultset %>%
          dplyr::mutate_all(as.character) %>%
          tidyr::pivot_longer(
            cols = !c(rowid, relationship_id),
            names_to = c("concept_attribute_type", "concept_order"),
            names_pattern = "(^.*)_([1-2]{1}$)",
            values_to = "concept_attribute",
            values_drop_na = TRUE
          ) %>%
          dplyr::distinct() %>%
          tidyr::unite(
            col = tooltip,
            concept_attribute_type,
            concept_attribute,
            sep = ": ",
            remove = TRUE,
            na.rm = TRUE
          ) %>%
          dplyr::group_by(rowid, concept_order) %>%
          dplyr::summarize_at(vars(tooltip), ~ paste(unique(.), collapse = "<br>")) %>%
          tidyr::pivot_wider(
            names_from = concept_order,
            names_prefix = "concept_tooltip_",
            values_from = tooltip
          ) %>%
          dplyr::ungroup()
      ) %>%
      dplyr::distinct()

    resultset3 <-
      resultset2 %>%
      tidyr::unite(
        col = concept_1,
        concept_id_1,
        concept_name_1,
        sep = " ",
        na.rm = TRUE,
        remove = FALSE
      ) %>%
      tidyr::unite(
        col = concept_2,
        concept_id_2,
        concept_name_2,
        sep = " ",
        na.rm = TRUE,
        remove = FALSE
      )

    resultset4 <-
      resultset3 %>%
      dplyr::mutate(color = relationship_id) %>%
      dplyr::mutate(color = factor(color))

    levels(resultset4$color) <- colorspace::sequential_hcl(n = length(levels(resultset4$color)))

    resultset5 <-
      resultset4 %>%
      dplyr::select(
        concept_1,
        concept_tooltip_1,
        relationship_id,
        concept_2,
        concept_tooltip_2,
        color
      )


    root <-
      resultset5 %>%
      dplyr::transmute(
        parent = NA,
        child = concept_1,
        tooltip = concept_tooltip_1,
        color = "black"
      ) %>%
      dplyr::distinct()

    level_1 <-
      resultset5 %>%
      dplyr::transmute(
        parent = concept_1,
        child = relationship_id,
        tooltip = "",
        color = color
      ) %>%
      dplyr::distinct()


    level_2 <-
      resultset5 %>%
      dplyr::transmute(
        parent = relationship_id,
        child = concept_2,
        tooltip = concept_tooltip_2,
        color = color
      ) %>%
      dplyr::distinct()



    final_a <-
      dplyr::bind_rows(
        root,
        level_1
      )



    final_b <-
      dplyr::bind_rows(level_2) %>%
      dplyr::group_by(child) %>%
      dplyr::mutate(total = n()) %>%
      dplyr::mutate(tally = 1:n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(append_label = ifelse(total > 1, paste0("(", tally, ")"), NA_character_)) %>%
      tidyr::unite(
        col = child2,
        child,
        append_label,
        sep = " ",
        remove = TRUE,
        na.rm = TRUE
      ) %>%
      dplyr::rename(child = child2) %>%
      dplyr::select(-total, -tally)

    final <-
      dplyr::bind_rows(
        final_a,
        final_b
      )

    collapsibleTree::collapsibleTreeNetwork(
      df = final,
      tooltipHtml = "tooltip",
      fill = "color"
    )
  }



plot_concept_relationships <-
  function(concept_ids,
           color_col,
           conn = NULL) {
    resultset <-
      queryAthena(
        sql_statement =
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
                                FROM @vocab_schema.concept_relationship cr
                                LEFT JOIN @vocab_schema.concept c1
                                ON c1.concept_id = cr.concept_id_1
                                LEFT JOIN @vocab_schema.concept c2
                                ON c2.concept_id = cr.concept_id_2
                                WHERE cr.concept_id_1 = (@concept_ids)
                                    AND cr.invalid_reason IS NULL
                                    AND c1.invalid_reason IS NULL
                                    AND c2.invalid_reason IS NULL
                                ",
            vocab_schema = vocab_schema,
            concept_ids = concept_ids
          ),
        conn = conn
      ) %>%
      tibble::rowid_to_column() %>%
      dplyr::mutate(rowid = as.character(rowid))

    resultset2 <-
      dplyr::left_join(
        resultset,
        resultset %>%
          dplyr::mutate_all(as.character) %>%
          tidyr::pivot_longer(
            cols = !c(rowid, relationship_id),
            names_to = c("concept_attribute_type", "concept_order"),
            names_pattern = "(^.*)_([1-2]{1}$)",
            values_to = "concept_attribute",
            values_drop_na = TRUE
          ) %>%
          dplyr::distinct() %>%
          tidyr::unite(
            col = tooltip,
            concept_attribute_type,
            concept_attribute,
            sep = ": ",
            remove = TRUE,
            na.rm = TRUE
          ) %>%
          dplyr::group_by(rowid, concept_order) %>%
          dplyr::summarize_at(vars(tooltip), ~ paste(unique(.), collapse = "<br>")) %>%
          tidyr::pivot_wider(
            names_from = concept_order,
            names_prefix = "concept_tooltip_",
            values_from = tooltip
          ) %>%
          dplyr::ungroup()
      ) %>%
      dplyr::distinct()

    resultset3 <-
      resultset2 %>%
      tidyr::unite(
        col = concept_1,
        concept_id_1,
        concept_name_1,
        sep = " ",
        na.rm = TRUE,
        remove = FALSE
      ) %>%
      tidyr::unite(
        col = concept_2,
        concept_id_2,
        concept_name_2,
        sep = " ",
        na.rm = TRUE,
        remove = FALSE
      )

    resultset4 <-
      resultset3 %>%
      dplyr::mutate(color = relationship_id) %>%
      dplyr::mutate(color = factor(color))

    levels(resultset4$color) <- colorspace::sequential_hcl(n = length(levels(resultset4$color)))

    resultset5 <-
      resultset4 %>%
      dplyr::select(
        concept_1,
        concept_tooltip_1,
        relationship_id,
        concept_2,
        concept_tooltip_2,
        color
      )


    root <-
      resultset5 %>%
      dplyr::transmute(
        parent = NA,
        child = concept_1,
        tooltip = concept_tooltip_1,
        color = "black"
      ) %>%
      dplyr::distinct()

    level_1 <-
      resultset5 %>%
      dplyr::transmute(
        parent = concept_1,
        child = relationship_id,
        tooltip = "",
        color = color
      ) %>%
      dplyr::distinct()


    level_2 <-
      resultset5 %>%
      dplyr::transmute(
        parent = relationship_id,
        child = concept_2,
        tooltip = concept_tooltip_2,
        color = color
      ) %>%
      dplyr::distinct()



    final_a <-
      dplyr::bind_rows(
        root,
        level_1
      )



    final_b <-
      dplyr::bind_rows(level_2) %>%
      dplyr::group_by(child) %>%
      dplyr::mutate(total = n()) %>%
      dplyr::mutate(tally = 1:n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(append_label = ifelse(total > 1, paste0("(", tally, ")"), NA_character_)) %>%
      tidyr::unite(
        col = child2,
        child,
        append_label,
        sep = " ",
        remove = TRUE,
        na.rm = TRUE
      ) %>%
      dplyr::rename(child = child2) %>%
      dplyr::select(-total, -tally)

    final <-
      dplyr::bind_rows(
        final_a,
        final_b
      )

    collapsibleTree::collapsibleTreeNetwork(
      df = final,
      tooltipHtml = "tooltip",
      fill = "color"
    )
  }





#' INNER JOIN an OMOP Vocabulary Table
#' @description This function executes the join() function with joinType == "INNER".
#' @export

innerJoin <-
  function(data,
           column = NULL,
           write_schema = "patelm9",
           vocab_schema = "omop_vocabulary",
           vocab_table,
           vocab_column,
           where_vocab_col = NULL,
           where_vocab_col_in = NULL,
           render_sql = TRUE,
           conn = NULL) {
    join(
      data = data,
      joinType = "INNER",
      column = column,
      write_schema = write_schema,
      vocab_schema = vocab_schema,
      vocab_table = vocab_table,
      vocab_column = vocab_column,
      where_vocab_col = where_vocab_col_in,
      where_vocab_col_in = where_vocab_col_in,
      render_sql = render_sql,
      conn = conn
    )
  }



#' #' @title
#' #' Join For Concept Synonyms
#' #'
#' #' @description
#' #' `join_for_*` functions differ from `join_on_*` functions in that `join_for_*`
#' #' joins on a vocabulary table field that is already specified, and that the join
#' #' is to add a specific field to the data, this case being the
#' #' `concept_synonym_name` field with a join on the `concept_id` field.
#' #'
#' #' @rdname join_for_concept_synonym_name
#' #' @export
#'
#' join_for_concept_synonym_name <-
#'   function(kind = c("LEFT", "RIGHT", "INNER", "FULL"),
#'            data,
#'            concept_id_column = NULL,
#'            select_data_columns = "*",
#'            select_concept_synonym_fields = c("concept_id", "concept_synonym_name"),
#'            distinct = FALSE,
#'            write_schema = "patelm9",
#'            vocab_schema = "omop_vocabulary",
#'            where_in_concept_synonym_field = "language_concept_id",
#'            where_in_concept_synonym_field_value = 4180186,
#'            where_not_in_concept_synonym_field,
#'            where_not_in_concept_synonym_field_value,
#'            where_is_null_concept_synonym_field,
#'            where_is_not_null_concept_synonym_field,
#'            case_insensitive = TRUE,
#'            conn,
#'            conn_fun = "connectAthena()",
#'            verbose = TRUE,
#'            render_sql = TRUE,
#'            render_only = FALSE
#'   ) {
#'
#'
#'     join(kind = kind,
#'          data = data,
#'          column = concept_id_column,
#'          vocab_table = "concept_synonym",
#'          vocab_field = "concept_id",
#'          select_data_columns = select_data_columns,
#'          select_vocab_fields = select_concept_synonym_fields,
#'          distinct = distinct,
#'          write_schema = write_schema,
#'          vocab_schema = vocab_schema,
#'          where_in_vocab_field = where_in_concept_synonym_field,
#'          where_in_vocab_field_value = where_in_concept_synonym_field_value,
#'          where_not_in_vocab_field = where_not_in_concept_synonym_field,
#'          where_not_in_vocab_field_value = where_not_in_concept_synonym_field_value,
#'          where_is_null_vocab_field = where_is_null_concept_synonym_field,
#'          where_is_not_null_vocab_field = where_is_not_null_concept_synonym_field,
#'          case_insensitive = case_insensitive,
#'          conn = conn,
#'          conn_fun = conn_fun,
#'          verbose = verbose,
#'          render_sql = render_sql,
#'          render_only = render_only)
#'   }
#'
#'
#'
#'
#' join_for_descendant_ids <-
#'   function(kind = c("LEFT", "RIGHT", "INNER", "FULL"),
#'            data,
#'            column = NULL,
#'            select_data_columns = "*",
#'            distinct = FALSE,
#'            write_schema = "patelm9",
#'            vocab_schema = "omop_vocabulary",
#'            where_in_concept_ancestor_field,
#'            where_in_concept_ancestor_field_value,
#'            where_not_in_concept_ancestor_field,
#'            where_not_in_concept_ancestor_field_value,
#'            where_is_null_concept_ancestor_field,
#'            where_is_not_null_concept_ancestor_field,
#'            case_insensitive = TRUE,
#'            conn,
#'            conn_fun = "connectAthena()",
#'            verbose = TRUE,
#'            render_sql = TRUE,
#'            render_only = FALSE
#'   ) {
#'
#'
#'     join(kind = kind,
#'          data = data,
#'          column = column,
#'          vocab_table = "concept_ancestor",
#'          vocab_field = "ancestor_concept_id",
#'          select_data_columns = select_data_columns,
#'          select_vocab_fields = c("descendant_concept_id", "min_levels_of_separation", "max_levels_of_separation"),
#'          distinct = distinct,
#'          write_schema = write_schema,
#'          vocab_schema = vocab_schema,
#'          where_in_vocab_field = where_in_concept_ancestor_field,
#'          where_in_vocab_field_value = where_in_concept_ancestor_field_value,
#'          where_not_in_vocab_field = where_not_in_concept_ancestor_field,
#'          where_not_in_vocab_field_value = where_not_in_concept_ancestor_field_value,
#'          where_is_null_vocab_field = where_is_null_concept_ancestor_field,
#'          where_is_not_null_vocab_field = where_is_not_null_concept_ancestor_field,
#'          case_insensitive = case_insensitive,
#'          conn = conn,
#'          conn_fun = conn_fun,
#'          verbose = verbose,
#'          render_sql = render_sql,
#'          render_only = render_only)
#'   }
#'
#'
#' join_for_ancestor_ids <-
#'   function(kind = c("LEFT", "RIGHT", "INNER", "FULL"),
#'            data,
#'            column = NULL,
#'            select_data_columns = "*",
#'            distinct = FALSE,
#'            write_schema = "patelm9",
#'            vocab_schema = "omop_vocabulary",
#'            where_in_concept_ancestor_field,
#'            where_in_concept_ancestor_field_value,
#'            where_not_in_concept_ancestor_field,
#'            where_not_in_concept_ancestor_field_value,
#'            where_is_null_concept_ancestor_field,
#'            where_is_not_null_concept_ancestor_field,
#'            case_insensitive = TRUE,
#'            conn,
#'            conn_fun = "connectAthena()",
#'            verbose = TRUE,
#'            render_sql = TRUE,
#'            render_only = FALSE
#'   ) {
#'
#'
#'     join(kind = kind,
#'          data = data,
#'          column = column,
#'          vocab_table = "concept_ancestor",
#'          vocab_field = "descendant_concept_id",
#'          select_data_columns = select_data_columns,
#'          select_vocab_fields = c("ancestor_concept_id", "min_levels_of_separation", "max_levels_of_separation"),
#'          distinct = distinct,
#'          write_schema = write_schema,
#'          vocab_schema = vocab_schema,
#'          where_in_vocab_field = where_in_concept_ancestor_field,
#'          where_in_vocab_field_value = where_in_concept_ancestor_field_value,
#'          where_not_in_vocab_field = where_not_in_concept_ancestor_field,
#'          where_not_in_vocab_field_value = where_not_in_concept_ancestor_field_value,
#'          where_is_null_vocab_field = where_is_null_concept_ancestor_field,
#'          where_is_not_null_vocab_field = where_is_not_null_concept_ancestor_field,
#'          case_insensitive = case_insensitive,
#'          conn = conn,
#'          conn_fun = conn_fun,
#'          verbose = verbose,
#'          render_sql = render_sql,
#'          render_only = render_only)
#'   }





#' LEFT JOIN an OMOP Vocabulary Table
#' @description
#' This function executes the join() function with joinType == "LEFT".
#' @export

leftJoin <-
  function(data,
           column = NULL,
           write_schema = "patelm9",
           vocab_schema = "omop_vocabulary",
           vocab_table,
           vocab_column,
           where_vocab_col = NULL,
           where_vocab_col_in = NULL,
           verbose = FALSE,
           conn = NULL,
           render_sql = FALSE,
           sleepTime = 1) {
    join(
      data = data,
      joinType = "LEFT",
      column = column,
      write_schema = write_schema,
      vocab_schema = vocab_schema,
      vocab_table = vocab_table,
      vocab_column = vocab_column,
      where_vocab_col = where_vocab_col,
      where_vocab_col_in = where_vocab_col_in,
      verbose = verbose,
      conn = conn,
      render_sql = render_sql,
      sleepTime = sleepTime
    )
  }




#' @title Join a dataframe object with the Concept Table
#' @description
#' This function has better performance than the WHERE IN statement for larger searches. A table in the format of "v{unpunctuated timestamp}" is written to the local Athena. A join is performed with a concept table. The table is subsequently dropped. If a dataframe column is not provided as a the column to join the concept table on, the 1st column will be used by default.
#'
#' @param data                 A data frame
#' @param column                Data frame column that the join will be performed on. If NULL, defaults to the column in position 1 of the data frame.
#' @param vocab_schema         Schema of the OMOP Concept Table
#' @param concept_column        Column in the concept
#' @param verbose               If TRUE, prints whether the cache is being loaded or being actively queried in the Postgres database, Default: FALSE
#' @param conn                  Connection object if another database is used. Default: NULL
#' @param render_sql            If TRUE, will print the SQL to the console before executing. Default: FALSE
#' @param sleepTime             Argument in seconds passed to the `Sys.sleep()` function at the end of query, Default: 1
#' @param ...                   Additional arguments passed to the `queryAthena()` function.
#'
#' @return
#' A data frame
#'
#' @seealso
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{filter}},\code{\link[dplyr]{mutate-joins}}
#'  \code{\link[rubix]{group_by_unique_aggregate}}
#' @rdname leftJoinConcept
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr select filter left_join
#' @importFrom rubix group_by_unique_aggregate


leftJoinConceptId <-
  function(data,
           column = NULL,
           write_schema,
           vocab_schema = "public",
           synonyms = FALSE,
           vocabulary_id,
           domain_id,
           concept_class_id,
           standard_concept,
           invalid_reason,
           verbose = FALSE,
           conn = NULL,
           render_sql = FALSE,
           sleepTime = 1) {
    if (is.null(column)) {
      column <- colnames(data)[1]
    }

    concept_column <- "concept_id"

    if (column == concept_column) {
      stop("'column' parameter cannot be equal to 'concept_column'")
    }


    concept_filters <- generate_concept_filters(
      vocabSchema = vocab_schema,
      vocabulary_id = vocabulary_id,
      domain_id = domain_id,
      concept_class_id = concept_class_id,
      standard_concept = standard_concept,
      invalid_reason = invalid_reason
    )


    # output <-
    #     leftJoin(data = data,
    #              column = column,
    #              vocab_schema = vocab_schema,
    #              vocab_table = "concept",
    #              vocab_column = concept_column,
    #              verbose = verbose,
    #              conn = conn,
    #              render_sql = render_sql,
    #              sleepTime = sleepTime)

    if (is.null(conn)) {
      write_conn <- connectAthena()
    } else {
      write_conn <- conn
    }

    temp_table <- make_temp_table_name()

    pg13::dropTable(
      conn = write_conn,
      schema = write_schema,
      tableName = temp_table
    )


    pg13::writeTable(
      conn = write_conn,
      schema = write_schema,
      tableName = temp_table,
      data
    )


    if (is.null(concept_filters)) {
      if (synonyms) {
        resultset <-
          queryAthena(
            SqlRender::render(
              "
                                                                WITH concepts AS (
                                                                    SELECT c.*
                                                                    FROM @write_schema.@temp_table temp
                                                                    INNER JOIN @vocab_schema.concept c
                                                                    ON c.@concept_column = temp.@column
                                                                ),
                                                                concept_synonyms AS (
                                                                    SELECT cs.concept_id, STRING_AGG(cs.concept_synonym_name, '|') AS concept_synonyms
                                                                    FROM @vocab_schema.concept_synonym cs
                                                                    INNER JOIN concepts c1
                                                                    ON c1.concept_id = cs.concept_id
                                                                    GROUP BY cs.concept_id
                                                                )

                                                                SELECT DISTINCT
                                                                        temp.*,
                                                                        c2.*,
                                                                        cs2.concept_synonyms
                                                                FROM @write_schema.@temp_table temp
                                                                LEFT JOIN concepts c2
                                                                ON c2.@concept_column = temp.@column
                                                                LEFT JOIN concept_synonyms cs2
                                                                ON c2.@concept_column = cs2.@concept_column
                                                                ",
              write_schema = write_schema,
              temp_table = temp_table,
              vocab_schema = vocab_schema,
              concept_column = concept_column,
              column = column
            ),
            conn = conn,
            skip_cache = TRUE,
            verbose = verbose,
            render_sql = render_sql,
            sleepTime = sleepTime
          )
      } else {
        resultset <-
          queryAthena(
            SqlRender::render(
              "
                                                                WITH concepts AS (
                                                                    SELECT c.*
                                                                    FROM @write_schema.@temp_table temp
                                                                    INNER JOIN @vocab_schema.concept c
                                                                    ON c.@concept_column = temp.@column
                                                                )

                                                                SELECT DISTINCT
                                                                        temp.*,
                                                                        c2.*
                                                                FROM @write_schema.@temp_table temp
                                                                LEFT JOIN concepts c2
                                                                ON c2.@concept_column = temp.@column
                                                                ",
              write_schema = write_schema,
              temp_table = temp_table,
              vocab_schema = vocab_schema,
              concept_column = concept_column,
              column = column
            ),
            conn = conn,
            skip_cache = TRUE,
            verbose = verbose,
            render_sql = render_sql,
            sleepTime = sleepTime
          )
      }
    } else {
      if (synonyms) {
        resultset <-
          queryAthena(
            SqlRender::render(
              "
                                                                        WITH concepts AS (
                                                                            SELECT @vocab_schema.concept.*
                                                                            FROM @write_schema.@temp_table temp
                                                                            INNER JOIN @vocab_schema.concept
                                                                            ON @vocab_schema.concept.@concept_column = temp.@column
                                                                            WHERE @concept_filters
                                                                        ),
                                                                        concept_synonyms AS (
                                                                            SELECT cs.concept_id, STRING_AGG(cs.concept_synonym_name, '|') AS concept_synonyms
                                                                            FROM @vocab_schema.concept_synonym cs
                                                                            INNER JOIN concepts c1
                                                                            ON c1.concept_id = cs.concept_id
                                                                            GROUP BY cs.concept_id
                                                                        )

                                                                        SELECT DISTINCT
                                                                                temp.*,
                                                                                c2.*,
                                                                                cs2.concept_synonyms
                                                                        FROM @write_schema.@temp_table temp
                                                                        LEFT JOIN concepts c2
                                                                        ON c2.@concept_column = temp.@column
                                                                        LEFT JOIN concept_synonyms cs2
                                                                        ON c2.@concept_column = cs2.@concept_column
                                                                        ",
              write_schema = write_schema,
              temp_table = temp_table,
              vocab_schema = vocab_schema,
              concept_column = concept_column,
              column = column,
              concept_filters = concept_filters
            ),
            conn = conn,
            skip_cache = TRUE,
            verbose = verbose,
            render_sql = render_sql,
            sleepTime = sleepTime
          )
      } else {
        resultset <-
          queryAthena(
            SqlRender::render(
              "
                                                                        WITH concepts AS (
                                                                            SELECT @vocab_schema.concept.*
                                                                            FROM @write_schema.@temp_table temp
                                                                            INNER JOIN @vocab_schema.concept
                                                                            ON @vocab_schema.concept.@concept_column = temp.@column
                                                                            WHERE @concept_filters
                                                                        )

                                                                        SELECT DISTINCT
                                                                                temp.*,
                                                                                c2.*
                                                                        FROM @write_schema.@temp_table temp
                                                                        LEFT JOIN concepts c2
                                                                        ON c2.@concept_column = temp.@column
                                                                        ",
              write_schema = write_schema,
              temp_table = temp_table,
              vocab_schema = vocab_schema,
              concept_column = concept_column,
              column = column,
              concept_filters = concept_filters
            ),
            conn = conn,
            skip_cache = TRUE,
            verbose = verbose,
            render_sql = render_sql,
            sleepTime = sleepTime
          )
      }
    }


    pg13::dropTable(
      conn = write_conn,
      schema = write_schema,
      tableName = temp_table
    )

    if (is.null(conn)) {
      dcAthena(conn = write_conn)
    }


    return(resultset)
  }


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param column PARAM_DESCRIPTION, Default: NULL
#' @param vocab_schema PARAM_DESCRIPTION, Default: 'public'
#' @param concept_synonym_column PARAM_DESCRIPTION, Default: 'concept_synonm_name'
#' @param verbose PARAM_DESCRIPTION, Default: FALSE
#' @param conn PARAM_DESCRIPTION, Default: NULL
#' @param render_sql PARAM_DESCRIPTION, Default: FALSE
#' @param sleepTime PARAM_DESCRIPTION, Default: 1
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @seealso
#'  \code{\link[stampede]{stamp_this}}
#'  \code{\link[pg13]{writeTable}}
#'  \code{\link[SqlRender]{render}}
#' @rdname leftJoinSynonymNames
#' @export
#' @importFrom stampede stamp_this
#' @importFrom pg13 writeTable
#' @importFrom SqlRender render

leftJoinSynonymNames <-
  function(data,
           column = NULL,
           write_schema = "public",
           verbose = FALSE,
           render_sql = FALSE,
           sleepTime = 1,
           vocabulary_id,
           domain_id,
           concept_class_id,
           standard_concept,
           invalid_reason,
           conn = NULL,
           omop_vocabulary_schema) {
    if (!is.null(conn)) {
      if (missing(omop_vocabulary_schema)) {
        stop("'omop_vocabulary_schema required to run query")
      }
    } else {
      omop_vocabulary_schema <- "public"
    }


    where_clauses <- vector()
    where_clauses_fields <- vector()
    if (!missing(vocabulary_id)) {
      where_clauses_fields <-
        c(
          where_clauses_fields,
          "vocabulary_id"
        )

      vocabulary_id <- paste0("'", vocabulary_id, "'")
      where_clauses <-
        c(
          where_clauses,
          SqlRender::render("@omop_vocabulary_schema.concept.vocabulary_id IN (@vocabulary_id)\n", vocabulary_id = vocabulary_id)
        )
    }

    if (!missing(domain_id)) {
      where_clauses_fields <-
        c(
          where_clauses_fields,
          "domain_id"
        )

      domain_id <- paste0("'", domain_id, "'")
      where_clauses <-
        c(
          where_clauses,
          SqlRender::render("@omop_vocabulary_schema.concept.domain_id IN (@domain_id)\n", domain_id = domain_id)
        )
    }

    if (!missing(concept_class_id)) {
      where_clauses_fields <-
        c(
          where_clauses_fields,
          "concept_class_id"
        )

      concept_class_id <- paste0("'", concept_class_id, "'")
      where_clauses <-
        c(
          where_clauses,
          SqlRender::render("@omop_vocabulary_schema.concept.concept_class_id IN (@concept_class_id)\n", concept_class_id = concept_class_id)
        )
    }

    if (!missing(standard_concept)) {
      where_clauses_fields <-
        c(
          where_clauses_fields,
          "standard_concept"
        )

      if (any("NULL" %in% standard_concept)) {
        part_a <- "@omop_vocabulary_schema.concept.standard_concept IS NULL"
      } else {
        part_a <- vector()
      }

      standard_concept <- standard_concept[!(standard_concept %in% "NULL")]

      if (length(standard_concept)) {
        standard_concept <- paste0("'", standard_concept, "'")

        part_b <- SqlRender::render("@omop_vocabulary_schema.concept.standard_concept IN (@standard_concept)", standard_concept = standard_concept)
      } else {
        part_b <- vector()
      }

      clause_with_null <- c(part_a, part_b) %>% paste(collapse = " OR ")

      where_clauses <-
        c(
          where_clauses,
          clause_with_null
        )
    }


    if (!missing(invalid_reason)) {
      where_clauses_fields <-
        c(
          where_clauses_fields,
          "invalid_reason"
        )

      if (any("NULL" %in% invalid_reason)) {
        part_a <- "@omop_vocabulary_schema.concept.invalid_reason IS NULL"
      } else {
        part_a <- vector()
      }

      invalid_reason <- invalid_reason[!(invalid_reason %in% "NULL")]

      if (length(invalid_reason)) {
        invalid_reason <- paste0("'", invalid_reason, "'")

        part_b <- SqlRender::render("@omop_vocabulary_schema.concept.invalid_reason IN (@invalid_reason)", invalid_reason = invalid_reason)
      } else {
        part_b <- vector()
      }

      clause_with_null <- c(part_a, part_b) %>% paste(collapse = " OR ")


      where_clauses <-
        c(
          where_clauses,
          clause_with_null
        )
    }

    if (length(where_clauses)) {
      where_clauses <- paste(where_clauses, collapse = " AND ")
    }


    table_name <- paste0("v", stampede::stamp_this(without_punct = TRUE))

    if (is.null(column)) {
      column <- colnames(data)[1]
    }


    if (is.null(conn)) {
      conn <- connectAthena()
      pg13::writeTable(
        conn = conn,
        schema = write_schema,
        tableName = table_name,
        data = data
      )
      dcAthena(conn = conn)


      if (length(where_clauses) == 0) {
        sql_statement <-
          SqlRender::render("SELECT *
                                                FROM @write_schema.@table_name a
                                                LEFT JOIN @omop_vocabulary_schema.concept_synonym cs
                                                ON LOWER(cs.concept_synonym_name) = LOWER(a.@column);",
            omop_vocabulary_schema = omop_vocabulary_schema,
            table_name = table_name,
            column = column,
            write_schema = write_schema
          )
      } else {
        sql_statement <-
          SqlRender::render(paste0(
            "
                                                WITH omop_concepts AS (
                                                            SELECT @omop_vocabulary_schema.concept_synonym.*
                                                            FROM @omop_vocabulary_schema.concept
                                                            INNER JOIN @omop_vocabulary_schema.concept_synonym
                                                            ON @omop_vocabulary_schema.concept_synonym.concept_id = @omop_vocabulary_schema.concept.concept_id
                                                            WHERE ", where_clauses,
            ")

                                                SELECT a.*, omop.*
                                                FROM @write_schema.@table_name a
                                                LEFT JOIN omop_concepts omop
                                                ON LOWER(omop.concept_synonym_name) = LOWER(a.@column)"
          ),
          omop_vocabulary_schema = omop_vocabulary_schema,
          table_name = table_name,
          column = column,
          write_schema = write_schema
          )
      }

      resultset <- queryAthena(
        sql_statement = sql_statement,
        verbose = verbose,
        skip_cache = TRUE,
        render_sql = render_sql,
        sleepTime = sleepTime
      )



      conn <- connectAthena()
      dropJoinTables(
        conn = conn,
        schema = write_schema
      )
      dcAthena(conn = conn)

      resultset
    } else {
      pg13::writeTable(
        conn = conn,
        schema = write_schema,
        tableName = table_name,
        data = data
      )

      if (length(where_clauses) == 0) {
        sql_statement <-
          SqlRender::render("SELECT *
                                                FROM @write_schema.@table_name a
                                                LEFT JOIN @omop_vocabulary_schema.concept_synonym cs
                                                ON LOWER(cs.concept_synonym_name) = LOWER(a.@column);",
            omop_vocabulary_schema = omop_vocabulary_schema,
            table_name = table_name,
            column = column,
            write_schema = write_schema
          )
      } else {
        sql_statement <-
          SqlRender::render(paste0(
            "
                    WITH omop_concepts AS (
                        SELECT @omop_vocabulary_schema.concept_synonym.*
                            FROM @omop_vocabulary_schema.concept
                        INNER JOIN @omop_vocabulary_schema.concept_synonym
                        ON @omop_vocabulary_schema.concept_synonym.concept_id = @omop_vocabulary_schema.concept.concept_id
                        WHERE ", where_clauses,
            ")

                        SELECT a.*, omop.*
                            FROM @write_schema.@table_name a
                        LEFT JOIN omop_concepts omop
                        ON LOWER(omop.concept_synonym_name) = LOWER(a.@column)"
          ),
          omop_vocabulary_schema = omop_vocabulary_schema,
          table_name = table_name,
          column = column,
          write_schema = write_schema
          )
      }

      resultset <- queryAthena(
        conn = conn,
        sql_statement = sql_statement,
        verbose = verbose,
        skip_cache = TRUE,
        render_sql = render_sql,
        sleepTime = sleepTime
      )

      dropJoinTables(
        conn = conn,
        schema = write_schema
      )

      resultset
    }
  }





#' @title Left Join a data frame to the Concept Ancestor Table
#' @param data PARAM_DESCRIPTION
#' @param vocab_schema PARAM_DESCRIPTION, Default: 'public'
#' @param descendant_id_column PARAM_DESCRIPTION, Default: NULL
#' @param whereLevelIn PARAM_DESCRIPTION, Default: NULL
#' @param whereLevelType PARAM_DESCRIPTION, Default: NULL
#' @param verbose PARAM_DESCRIPTION, Default: FALSE
#' @param conn PARAM_DESCRIPTION, Default: NULL
#' @param render_sql PARAM_DESCRIPTION, Default: FALSE
#' @param sleepTime PARAM_DESCRIPTION, Default: 1
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @seealso
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{distinct}},\code{\link[dplyr]{mutate-joins}}
#'  \code{\link[rubix]{rename_all_with_prefix}}
#' @rdname leftJoinForAncestors
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr select distinct left_join
#' @importFrom rubix rename_all_with_prefix

leftJoinForAncestors <-
  function(data,
           vocab_schema = "public",
           descendant_id_column = NULL,
           whereLevelIn = NULL,
           whereLevelType = NULL,
           verbose = FALSE,
           conn = NULL,
           render_sql = FALSE,
           sleepTime = 1,
           ...) {
    if (!is.null(whereLevelIn) && length(whereLevelType) != 1) {
      warning("No 'whereLevelType'. Defaulting to 'max'")
      whereLevelType <- "max"
    }

    if (!is.null(whereLevelIn)) {
      if (whereLevelType == "max") {
        whereAthenaField <- "max_levels_of_separation"
      } else {
        whereAthenaField <- "min_levels_of_separation"
      }


      ancestors <-
        leftJoin(
          data = data,
          column = descendant_id_column,
          vocab_schema = vocab_schema,
          vocab_table = "concept_ancestor",
          vocab_column = "descendant_concept_id",
          where_vocab_col = whereAthenaField,
          where_vocab_col_in = whereLevelIn,
          verbose = verbose,
          conn = conn,
          render_sql = render_sql,
          sleepTime = sleepTime,
          ...
        )
    } else {
      ancestors <-
        leftJoin(
          data = data,
          column = descendant_id_column,
          vocab_schema = vocab_schema,
          vocab_table = "concept_ancestor",
          vocab_column = "descendant_concept_id",
          verbose = verbose,
          conn = conn,
          render_sql = render_sql,
          sleepTime = sleepTime,
          ...
        )
    }



    ancestors_detail <-
      leftJoinConcept(ancestors %>%
        dplyr::select(ancestor_concept_id),
      vocab_schema = vocab_schema,
      synonyms = FALSE,
      verbose = verbose,
      conn = conn,
      render_sql = render_sql,
      sleepTime = sleepTime,
      ...
      ) %>%
      dplyr::select(-ancestor_concept_id) %>%
      rubix::rename_all_with_prefix("ancestor_") %>%
      dplyr::distinct()


    final_ancestors <-
      dplyr::left_join(ancestors,
        ancestors_detail,
        by = "ancestor_concept_id"
      ) %>%
      dplyr::select(-descendant_concept_id)


    return(final_ancestors)
  }




#' LEFT JOIN the Concept Parent Table
#' @export

leftJoinFoChildren <-
  function(data,
           vocab_schema,
           parent_id_column = NULL,
           render_sql = TRUE,
           conn = NULL) {
    leftJoin(
      data = data,
      column = parent_id_column,
      vocab_schema = vocab_schema,
      vocab_table = "concept_parent",
      vocab_column = "parent_concept_id",
      render_sql = render_sql,
      conn = conn
    ) %>%
      dplyr::filter(parent_concept_id != child_concept_id)
  }




#' @title Left Join a data frame to the Concept Ancestor Table
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param vocab_schema Default: 'public'
#' @param ancestor_id_column Default: NULL
#' @param whereLevelIn Default: NULL
#' @param whereLevelType Default: NULL
#' @param render_sql Default: TRUE
#' @param conn Default: NULL
#' @seealso
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{distinct}},\code{\link[dplyr]{mutate-joins}}
#'  \code{\link[rubix]{rename_all_with_prefix}}
#' @rdname leftJoinForDescendants
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr select distinct left_join
#' @importFrom rubix rename_all_with_prefix

leftJoinForDescendants <-
  function(data,
           write_schema = "patelm9",
           vocab_schema = "omop_vocabulary",
           ancestor_id_column = NULL,
           whereLevelIn = NULL,
           whereLevelType = NULL,
           verbose = TRUE,
           conn,
           render_sql = TRUE,
           sleepTime = 1) {
    if (!is.null(whereLevelIn) && length(whereLevelType) != 1) {
      warning("No 'whereLevelType'. Defaulting to 'max'")
      whereLevelType <- "max"
    }

    # Make sure concept id column is integer
    if (is.null(ancestor_id_column)) {
      ancestor_id_column <- colnames(data)[1]
    }

    data <-
      data %>%
      dplyr::mutate_at(dplyr::vars(dplyr::all_of(ancestor_id_column)), as.integer)

    if (!is.null(whereLevelIn)) {
      if (whereLevelType == "max") {
        whereAthenaField <- "max_levels_of_separation"
      } else {
        whereAthenaField <- "min_levels_of_separation"
      }


      descendants <-
        join(
          data = data,
          joinType = "LEFT",
          column = ancestor_id_column,
          write_schema = write_schema,
          vocab_schema = vocab_schema,
          vocab_table = "CONCEPT_ANCESTOR",
          vocab_column = "ancestor_concept_id",
          where_vocab_col = whereAthenaField,
          where_vocab_col_in = whereLevelIn,
          verbose = verbose,
          conn = conn,
          render_sql = render_sql,
          sleepTime = sleepTime
        )
    } else {
      descendants <-
        join(
          data = data,
          joinType = "LEFT",
          column = ancestor_id_column,
          write_schema = write_schema,
          vocab_schema = vocab_schema,
          vocab_table = "CONCEPT_ANCESTOR",
          vocab_column = "ancestor_concept_id",
          verbose = verbose,
          conn = conn,
          render_sql = render_sql,
          sleepTime = sleepTime
        )
    }


    descendants_detail <-
      join(
        data = descendants,
        joinType = "LEFT",
        column = "descendant_concept_id",
        write_schema = write_schema,
        vocab_schema = vocab_schema,
        vocab_table = "CONCEPT",
        vocab_column = "concept_id",
        verbose = verbose,
        conn = conn,
        conn_fun = conn_fun,
        render_sql = render_sql,
        sleepTime = sleepTime
      ) %>%
      dplyr::select(-descendant_concept_id) %>%
      rubix::rename_all_with_prefix("descendant_") %>%
      dplyr::distinct()


    final_descendants <-
      dplyr::left_join(descendants,
        descendants_detail,
        by = "descendant_concept_id"
      ) %>%
      dplyr::select(-ancestor_concept_id)


    final_descendants
  }




#' LEFT JOIN the Concept Parent Table
#' @export

leftJoinForParents <-
  function(data,
           vocab_schema,
           child_id_column = NULL,
           render_sql = TRUE,
           conn = NULL) {
    leftJoin(
      data = data,
      column = child_id_column,
      vocab_schema = vocab_schema,
      vocab_table = "concept_parent",
      vocab_column = "parent_concept_id",
      render_sql = render_sql,
      conn = conn
    ) %>%
      dplyr::filter(parent_concept_id != child_concept_id)
  }




#' Left Join Relationship
#' @export

leftJoinRelationship <-
  function(data,
           column = NULL,
           vocab_schema = "public",
           render_sql = TRUE,
           conn = NULL) {
    if (is.null(column)) {
      column <- colnames(data)[1]
    }



    .output1 <-
      leftJoin(
        data = data %>%
          dplyr::select(all_of(column)),
        vocab_schema = vocab_schema,
        vocab_table = "concept_relationship",
        vocab_column = "concept_id_1",
        render_sql = render_sql,
        conn = conn
      ) %>%
      dplyr::filter(is.na(invalid_reason)) %>%
      dplyr::select(
        -valid_start_date,
        -valid_end_date,
        -invalid_reason
      )

    .output1 <-
      dplyr::left_join(data,
        .output1,
        by = column
      )

    .output2 <-
      leftJoinConcept(
        data = .output1 %>%
          dplyr::select(concept_id_2),
        vocab_schema = vocab_schema,
        render_sql = render_sql,
        conn = conn
      ) %>%
      dplyr::select(-concept_id_2) %>%
      rubix::rename_all_suffix(suffix = "_2")


    dplyr::left_join(.output1,
      .output2,
      by = "concept_id_2"
    ) %>%
      dplyr::select(
        !ends_with("_2"),
        ends_with("_2"),
        dplyr::everything()
      )
  }




#' LEFT JOIN All Relatives
#' @import rubix
#' @import dplyr
#' @export

leftJoinRelatives <-
  function(data,
           vocab_schema = "public",
           id_column = NULL,
           whereLevelIn = NULL,
           whereLevelType = NULL,
           render_sql = TRUE,
           conn = NULL) {
    ancestors <-
      leftJoinForAncestors(
        data = data,
        vocab_schema = vocab_schema,
        descendant_id_column = id_column,
        whereLevelIn = whereLevelIn,
        whereLevelType = whereLevelType,
        render_sql = render_sql,
        conn = conn
      )

    descendants <-
      leftJoinForDescendants(
        data = data,
        vocab_schema = vocab_schema,
        ancestor_id_column = id_column,
        whereLevelIn = whereLevelIn,
        whereLevelType = whereLevelType,
        render_sql = render_sql,
        conn = conn
      )

    final <- list(
      A = ancestors,
      D = descendants
    ) %>%
      rubix::map_names_set(function(x) {
        x %>%
          rubix::rename_all_remove(pattern = "ancestor_|descendant_")
      }) %>%
      dplyr::bind_rows(.id = "relative_type") %>%
      rubix::rename_at_prefix(concept_id,
        concept_name,
        domain_id,
        vocabulary_id,
        concept_class_id,
        standard_concept,
        concept_code,
        valid_start_date,
        valid_end_date,
        invalid_reason,
        prefix = "relative_"
      ) %>%
      dplyr::select(
        all_of(colnames(data)),
        relative_type,
        min_levels_of_separation,
        max_levels_of_separation,
        relative_concept_id,
        relative_concept_name,
        relative_domain_id,
        relative_vocabulary_id,
        relative_concept_class_id,
        relative_standard_concept,
        relative_concept_code,
        relative_valid_start_date,
        relative_valid_end_date,
        relative_invalid_reason,
        dplyr::everything()
      )


    return(final)
  }




#' @title Join a data frame with the Concept Synonym Table
#' @description
#' This function has better performance than the WHERE IN statement for larger searches. A table in the format of "v{unpunctuated timestamp}" is written to the local Athena. A join is performed with a concept table. The table is subsequently dropped. If a dataframe column is not provided as a the column to join the concept table on, the 1st column will be used by default.
#'
#' @param data                 A data frame
#' @param column                Data frame column that the join will be performed on. If NULL, defaults to the column in position 1 of the data frame.
#' @param vocab_schema         Schema of the OMOP Vocabulary Tables
#' @param verbose               If TRUE, prints whether the cache is being loaded or being actively queried in the Postgres database, Default: FALSE
#' @param conn                  PARAM_DESCRIPTION, Default: NULL
#' @param render_sql            If TRUE, will print the SQL to the console before executing. Default: FALSE
#' @param sleepTime             Argument in seconds passed to the `Sys.sleep()` function at the end of query, Default: 1
#' @param ...                   Additional arguments passed to the `queryAthena()` function.
#'
#' @return
#' A data frame
#'
#' @seealso
#'  \code{\link[dplyr]{select}}
#' @rdname leftJoinSynonymId
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr select


leftJoinSynonymId <-
  function(data,
           column = NULL,
           vocab_schema,
           verbose = FALSE,
           conn = NULL,
           render_sql = FALSE,
           sleepTime = 1) {
    if (is.null(column)) {
      column <- colnames(data)[1]
    }


    if (column == "concept_id") {
      stop("'column' parameter cannot be equal to 'concept_id'")
    }


    leftJoin(
      data = data,
      column = column,
      vocab_schema = vocab_schema,
      vocab_table = "concept_synonym",
      vocab_column = "concept_id",
      render_sql = render_sql,
      where_vocab_col = "language_concept_id",
      where_vocab_col_in = 4180186,
      verbose = verbose,
      conn = conn,
      sleepTime = sleepTime
    ) %>%
      dplyr::select(-language_concept_id)
  }





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
      queryAthena(
        sql_statement =
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
            vocabulary_id = vocabulary_id
          ),
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
      queryAthena(
        sql_statement =
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
            domain_id = domain_id
          ),
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
      queryAthena(
        sql_statement =
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
            concept_id = concept_id
          ),
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
      tidyr::unite(
        col = child,
        concept_id,
        concept_name,
        sep = " ",
        na.rm = TRUE,
        remove = FALSE
      ) %>%
      dplyr::select(
        parent,
        child,
        dplyr::everything()
      )

    range_output <- list()
    range_output[[1]] <- level_1


    range <- 1:100
    for (i in 2:max(range)) {
      new_parents <-
        range_output[[i - 1]] %>%
        dplyr::select(concept_id) %>%
        dplyr::distinct() %>%
        unlist() %>%
        as.integer()

      level_n <-
        queryAthena(
          sql_statement =
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
              new_parents = new_parents
            ),
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

    secretary::typewrite("There are", length(range_output), "levels below", secretary::inside_out(sprintf("%s %s", concept_class_obj@concept_id, concept_class_obj@concept_name)))
    secretary::typewrite("Row counts:")
    1:length(range_output) %>%
      purrr::map2(
        range_output,
        function(x, y) {
          secretary::typewrite(sprintf("%s: %s", x, nrow(y)), tabs = 4, timepunched = FALSE)
        }
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
      tibble::tibble(
        parent = NA_character_,
        child = child
      )


    range_output <- preview_atc_classification(
      conn = conn,
      concept_class_obj = concept_class_obj,
      vocab_schema = vocab_schema,
      verbose = verbose,
      render_sql = render_sql,
      sleep_time = sleep_time
    )

    if (!missing(range)) {
      range_output <- range_output[range]
    }

    df <- dplyr::bind_rows(
      root,
      range_output
    )

    tooltip <-
      df %>%
      dplyr::mutate_all(as.character) %>%
      tidyr::pivot_longer(
        cols = !c(parent, child),
        names_to = "attribute",
        values_to = "attribute_value",
        values_drop_na = TRUE
      ) %>%
      tidyr::unite(
        col = tooltip,
        attribute,
        attribute_value,
        sep = ": ",
        remove = TRUE,
        na.rm = TRUE
      ) %>%
      dplyr::distinct() %>%
      dplyr::group_by(child) %>%
      dplyr::summarize_at(dplyr::vars(tooltip), ~ paste(., collapse = "<br>")) %>%
      dplyr::ungroup() %>%
      dplyr::distinct()

    color <- unlist(df[, color_by])
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
        collapsibleTree::collapsibleTreeNetwork(
          df = df,
          tooltipHtml = "tooltip",
          fill = "color"
        )
      } else {
        p <- collapsibleTree::collapsibleTreeNetwork(
          df = df,
          tooltipHtml = "tooltip",
          fill = "color"
        )

        htmlwidgets::saveWidget(
          widget = p,
          file = file
        )
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
      queryAthena(
        sql_statement =
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
            concept_id = concept_id
          ),
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
      tidyr::unite(
        col = child,
        concept_id,
        concept_name,
        sep = " ",
        na.rm = TRUE,
        remove = FALSE
      ) %>%
      dplyr::select(
        parent,
        child,
        dplyr::everything()
      )

    range_output <- list()
    range_output[[1]] <- level_1


    range <- 1:100
    for (i in 2:max(range)) {
      new_parents <-
        range_output[[i - 1]] %>%
        dplyr::select(concept_id) %>%
        dplyr::distinct() %>%
        unlist() %>%
        as.integer()

      level_n <-
        queryAthena(
          sql_statement =
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
              new_parents = new_parents
            ),
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

    secretary::typewrite("There are", length(range_output), "levels above", secretary::inside_out(sprintf("%s %s", concept_obj@concept_id, concept_obj@concept_name)))
    secretary::typewrite("Row counts:")
    1:length(range_output) %>%
      purrr::map2(
        range_output,
        function(x, y) {
          secretary::typewrite(sprintf("%s: %s", x, nrow(y)), tabs = 4, timepunched = FALSE)
        }
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
      tibble::tibble(
        parent = NA_character_,
        child = child
      )


    range_output <- preview_atc_classification(
      conn = conn,
      concept_obj = concept_obj,
      vocab_schema = vocab_schema,
      verbose = verbose,
      render_sql = render_sql,
      sleep_time = sleep_time
    )

    if (!missing(range)) {
      range_output <- range_output[range]
    }

    df <- dplyr::bind_rows(
      root,
      range_output
    )

    tooltip <-
      df %>%
      dplyr::mutate_all(as.character) %>%
      tidyr::pivot_longer(
        cols = !c(parent, child),
        names_to = "attribute",
        values_to = "attribute_value",
        values_drop_na = TRUE
      ) %>%
      tidyr::unite(
        col = tooltip,
        attribute,
        attribute_value,
        sep = ": ",
        remove = TRUE,
        na.rm = TRUE
      ) %>%
      dplyr::distinct() %>%
      dplyr::group_by(child) %>%
      dplyr::summarize_at(dplyr::vars(tooltip), ~ paste(., collapse = "<br>")) %>%
      dplyr::ungroup() %>%
      dplyr::distinct()

    color <- unlist(df[, color_by])
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
      collapsibleTree::collapsibleTreeNetwork(
        df = df,
        tooltipHtml = "tooltip",
        fill = "color"
      )
    } else {
      p <- collapsibleTree::collapsibleTreeNetwork(
        df = df,
        tooltipHtml = "tooltip",
        fill = "color"
      )

      htmlwidgets::saveWidget(
        widget = p,
        file = file
      )
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
      queryAthena(
        sql_statement =
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
            concept_id = concept_id
          ),
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
      tidyr::unite(
        col = child,
        concept_id,
        concept_name,
        sep = " ",
        na.rm = TRUE,
        remove = FALSE
      ) %>%
      dplyr::select(
        parent,
        child,
        dplyr::everything()
      )

    range_output <- list()
    range_output[[1]] <- level_1


    range <- 1:100
    for (i in 2:max(range)) {
      new_parents <-
        range_output[[i - 1]] %>%
        dplyr::select(concept_id) %>%
        dplyr::distinct() %>%
        unlist() %>%
        as.integer()

      level_n <-
        queryAthena(
          sql_statement =
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
              new_parents = new_parents
            ),
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

    secretary::typewrite("There are", length(range_output), "levels below", secretary::inside_out(sprintf("%s %s", concept_class_obj@concept_id, concept_class_obj@concept_name)))
    secretary::typewrite("Row counts:")
    1:length(range_output) %>%
      purrr::map2(
        range_output,
        function(x, y) {
          secretary::typewrite(sprintf("%s: %s", x, nrow(y)), tabs = 4, timepunched = FALSE)
        }
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
      tibble::tibble(
        parent = NA_character_,
        child = child
      )


    range_output <- preview_loinc_classification(
      conn = conn,
      concept_class_obj = concept_class_obj,
      vocab_schema = vocab_schema,
      verbose = verbose,
      render_sql = render_sql,
      sleep_time = sleep_time
    )

    if (!missing(range)) {
      range_output <- range_output[range]
    }

    df <- dplyr::bind_rows(
      root,
      range_output
    )

    tooltip <-
      df %>%
      dplyr::mutate_all(as.character) %>%
      tidyr::pivot_longer(
        cols = !c(parent, child),
        names_to = "attribute",
        values_to = "attribute_value",
        values_drop_na = TRUE
      ) %>%
      tidyr::unite(
        col = tooltip,
        attribute,
        attribute_value,
        sep = ": ",
        remove = TRUE,
        na.rm = TRUE
      ) %>%
      dplyr::distinct() %>%
      dplyr::group_by(child) %>%
      dplyr::summarize_at(dplyr::vars(tooltip), ~ paste(., collapse = "<br>")) %>%
      dplyr::ungroup() %>%
      dplyr::distinct()

    color <- unlist(df[, color_by])
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
        collapsibleTree::collapsibleTreeNetwork(
          df = df,
          tooltipHtml = "tooltip",
          fill = "color"
        )
      } else {
        p <- collapsibleTree::collapsibleTreeNetwork(
          df = df,
          tooltipHtml = "tooltip",
          fill = "color"
        )

        htmlwidgets::saveWidget(
          widget = p,
          file = file
        )
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
    queryConceptId(
      concept_ids = concept_id,
      schema = schema
    ) %>%
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
      unmergeStrip(
        strip_col = {{ strip_col }},
        remove = remove
      )
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
      tidyr::extract(
        col = {{ label_col }},
        into = c("concept_id", "concept_name"),
        regex = "(^.*?) (.*$)",
        remove = remove
      )
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
      dplyr::select(
        filterAtStripId,
        all_of(merge_cols)
      ) %>%
      tidyr::pivot_longer(
        cols = !filterAtStripId,
        names_to = "merge_col",
        values_to = "Concept",
        values_drop_na = TRUE
      )

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
        dplyr::select(
          filterAtStripId,
          merge_col,
          concept_id
        ) %>%
        dplyr::distinct() %>%
        dplyr::filter(!is.na(concept_id)) %>%
        tidyr::pivot_wider(
          id_cols = filterAtStripId,
          names_from = merge_col,
          values_from = concept_id,
          values_fn = list(concept_id = function(x) length(unique(x)))
        ) %>%
        dplyr::filter_at(dplyr::vars(!filterAtStripId), dplyr::all_vars(!is.na(.)))


      return(reserveData[(reserveData$filterAtStripId %in% inputData5$filterAtStripId), ] %>%
        dplyr::select(-contains("filterAtStripId")))
    } else {
      return(
        reserveData[(reserveData$filterAtStripId %in% inputData4$filterAtStripId), ] %>%
          dplyr::select(-contains("filterAtStripId"))
      )
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


    column_names <- c(
      "concept_id",
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


    if (any(column_names %in% colnames(data))) {
      qa <- column_names[column_names %in% colnames(data)]

      stop("data cannot have any concept table column names: ", paste(qa, collapse = ", "))
    }

    .output <-
      data %>%
      dplyr::mutate(!!tmp_col := !!merge_col) %>%
      separateConceptStrip(!!tmp_col) %>%
      # tidyr::separate_rows(!!tmp_col,
      #                      sep = "\n") %>%
      rubix::normalize_all_to_na() %>%
      dplyr::filter_at(dplyr::vars(!!tmp_col), dplyr::all_vars(!is.na(.))) %>%
      unmergeStrip(
        strip_col = !!tmp_col,
        remove = FALSE
      ) %>%
      dplyr::filter(...) %>%
      dplyr::select(-any_of(column_names)) %>%
      dplyr::select(-!!tmp_col) %>%
      dplyr::distinct()

    qa <- nrow(.output) > nrow(data)

    if (qa) {
      warning("returned data has more rows than input data")
    }

    return(.output)
  }




#' Get Merged Concept Id
#' @importFrom dplyr select
#' @noRd


getLabel <-
  function(concept_id,
           schema = "public") {
    queryConceptId(
      concept_ids = concept_id,
      schema = schema
    ) %>%
      makeLabel(
        into = "Label",
        remove = TRUE
      ) %>%
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
      tidyr::extract(
        col = !!label_col,
        into = c("concept_id", "concept_name"),
        regex = "(^.*?) (.*$)",
        remove = remove
      ) %>%
      dplyr::rename(label_concept_id = concept_id) %>%
      left_join_concept(
        column = "label_concept_id",
        include_synonyms = FALSE
      ) %>%
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
      tidyr::unite(
        col = {{ into }},
        dplyr::all_of(label_parts$concept_id),
        dplyr::all_of(label_parts$concept_name),
        sep = " ",
        na.rm = TRUE,
        remove = remove
      ) %>%
      dplyr::mutate_at(dplyr::vars({{ into }}), ~ na_if(., "NA NA"))
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
    column_names <- paste0(
      prefix,
      c(
        "concept_id",
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
      ),
      suffix
    ) %>%
      as.list()

    names(column_names) <- c(
      "concept_id",
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
      tidyr::unite(
        col = !!into,
        all_of(c(
          column_names$concept_id,
          column_names$concept_name
        )),
        sep = " ",
        remove = FALSE
      )


    # If All NA concepts are not merged into a strip and returns a single NA
    output <-
      output %>%
      dplyr::mutate_at(
        dplyr::vars(!!into),
        function(x) {
          ifelse(grepl("NA NA",
            x,
            ignore.case = FALSE
          ),
          NA_character_,
          x
          )
        }
      )


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
      warning(nrow(qa), " where concept id is not <NA>, but label is <NA>. See flagMergeLabel object.")
    }

    if (remove) {
      column_names <- paste0(
        prefix,
        c(
          "concept_id",
          "concept_name",
          "domain_id",
          "vocabulary_id",
          "concept_class_id",
          "standard_concept",
          "concept_code",
          "valid_start_date",
          "valid_end_date",
          "invalid_reason"
        ),
        suffix
      ) %>%
        as.list()

      names(column_names) <- c(
        "concept_id",
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
        dplyr::select_at(dplyr::vars(!any_of(c(
          column_names$concept_id,
          column_names$concept_name,
          column_names$domain_id,
          column_names$vocabulary_id,
          column_names$concept_class_id,
          column_names$standard_concept,
          column_names$concept_code,
          column_names$valid_start_date,
          column_names$valid_end_date,
          column_names$invalid_reason
        ))))
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
    column_names <- paste0(
      prefix,
      c(
        "concept_id",
        "concept_name",
        "domain_id",
        "vocabulary_id",
        "concept_class_id",
        "standard_concept",
        "concept_code",
        "valid_start_date",
        "valid_end_date",
        "invalid_reason"
      ),
      suffix
    ) %>%
      as.list()

    concept_fields <- c(
      "concept_id",
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
      tidyr::unite(
        col = vocabulary,
        dplyr::all_of(c(
          column_names$vocabulary_id,
          column_names$concept_code
        )),
        sep = " "
      ) %>%
      dplyr::mutate_at(
        dplyr::vars(dplyr::all_of(c(
          column_names$domain_id,
          "vocabulary",
          column_names$concept_class_id
        ))),
        function(x) paste0("[", x, "]")
      ) %>%
      # dplyr::select_at(dplyr::vars(!matches("valid.*date"))) %>%
      tidyr::unite(
        col = {{ into }},
        all_of(c(
          column_names$invalid_reason,
          column_names$standard_concept,
          column_names$concept_id,
          column_names$concept_name,
          "vocabulary",
          column_names$domain_id,
          column_names$concept_class_id
        )),
        sep = " ",
        remove = FALSE
      ) %>%
      dplyr::select(
        !!into_id_colname := all_of(column_names$concept_id),
        {{ into }}
      )


    # If All NA concepts are not merged into a strip and returns a single NA
    output <-
      output %>%
      dplyr::mutate_at(
        dplyr::vars({{ into }}),
        function(x) {
          ifelse(grepl("NA NA \\[NA NA\\] \\[NA\\] \\[NA\\]",
            x,
            ignore.case = FALSE
          ),
          NA_character_,
          x
          )
        }
      )


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
      warning(nrow(qa), " where concept id is not <NA>, but merge strip is <NA>. See flagMergeStrip object.")
    }



    if (!missing(...)) {
      output <-
        dplyr::bind_cols(
          output,
          data %>%
            dplyr::select(!!!preserve_cols)
        )
    }




    if (length(other_cols)) {
      output <-
        dplyr::bind_cols(
          output,
          data %>%
            dplyr::select(dplyr::all_of(other_cols))
        )
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
      tidyr::extract(
        col = !!label_col,
        into = c("concept_id", "concept_name"),
        regex = "(^.*?) (.*$)",
        remove = remove
      )
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
      sep = "(?<=\\])\n(?=\\[A-Z\\])"
    )
  }




#' Convert a Merge Strip to a Label
#' @noRd

stripToLabel <-
  function(data,
           merge_col,
           into,
           remove = FALSE) {
    unmergeStrip(
      dataframe = data,
      concept_col = {{ merge_col }},
      remove = remove
    ) %>%
      makeLabel(
        into = {{ into }},
        remove = remove
      )
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

    colOrder <- c(
      "invalid_reason",
      "standard_concept",
      "concept_id",
      "concept_name",
      "vocabulary_id",
      "concept_code",
      "domain_id",
      "concept_class_id"
    )

    new_cols <- paste0(
      add_prefix,
      colOrder,
      add_suffix
    ) %>%
      as.list()

    names(new_cols) <- colOrder

    new_cols <- new_cols

    if (any(unlist(new_cols) %in% colnames(data))) {
      qa <- unlist(new_cols)[unlist(new_cols) %in% colnames(data)]
      stop("new column names already present: ", paste(qa, collapse = ", "))
    }

    output <-
      data %>%
      tidyr::extract(
        col = !!strip_col,
        remove = FALSE,
        into = unlist(new_cols),
        regex = "(\\[.{1}\\]) (\\[.{1}\\]) ([^ ]*) (.*?) (\\[.*?) (.*?\\]) (\\[.*?\\]) (\\[.*?\\])"
      ) %>%
      tibble::as_tibble() %>%
      rubix::normalize_all_to_na()

    output <-
      output %>%
      dplyr::mutate_at(dplyr::vars(dplyr::all_of(unlist(new_cols))), stringr::str_remove_all, "^\\[|\\]$") %>%
      dplyr::mutate_at(dplyr::vars(new_cols$standard_concept, new_cols$invalid_reason), stringr::str_replace_all, "^N$|^V$", NA_character_) %>%
      dplyr::select(
        dplyr::all_of(c(
          new_cols$concept_id,
          new_cols$concept_name,
          new_cols$domain_id,
          new_cols$vocabulary_id,
          new_cols$concept_class_id,
          new_cols$standard_concept,
          new_cols$concept_code,
          new_cols$invalid_reason
        )),
        dplyr::everything()
      )

    if (r_trimws == TRUE) {
      output <-
        output %>%
        dplyr::mutate_at(
          dplyr::vars(dplyr::all_of(c(
            new_cols$concept_id,
            new_cols$concept_name,
            new_cols$domain_id,
            new_cols$vocabulary_id,
            new_cols$concept_class_id,
            new_cols$standard_concept,
            new_cols$concept_code,
            new_cols$invalid_reason
          ))),
          base::trimws
        )
    }

    qa <-
      output %>%
      dplyr::filter_at(
        dplyr::vars(c(
          new_cols$concept_id,
          new_cols$concept_name,
          new_cols$domain_id,
          new_cols$vocabulary_id,
          new_cols$concept_class_id,
          new_cols$standard_concept,
          new_cols$concept_code,
          new_cols$invalid_reason
        )),
        dplyr::all_vars(is.na(.))
      ) %>%
      dplyr::filter_at(
        dplyr::vars(!!strip_col),
        dplyr::all_vars(!is.na(.))
      )

    if (nrow(qa) > 0) {
      flagUnmergeStrip <<- qa

      warning("Not all concepts unmerged: ", nrow(qa), ". See flagUnmergeStrip object.")
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

    x <- queryConceptId(
      concept_ids = concept_id,
      schema = schema,
      override_cache = TRUE
    )

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
      pg13::build_query(
        fields = c("domain_id", "vocabulary_id", "concept_class_id"),
        distinct = TRUE,
        schema = schema,
        tableName = "concept"
      )

    queryAthena(
      sql_statement = sql_statement,
      verbose = verbose,
      cache_resultset = cache_resultset,
      override_cache = override_cache,
      conn = conn,
      render_sql = render_sql,
      sleepTime = sleepTime,
      ...
    )
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
      pg13::build_query(
        fields = c("domain_id"),
        distinct = TRUE,
        schema = schema,
        tableName = "concept"
      )

    queryAthena(
      sql_statement = sql_statement,
      verbose = verbose,
      cache_resultset = cache_resultset,
      override_cache = override_cache,
      conn = conn,
      render_sql = render_sql,
      sleepTime = sleepTime,
      ...
    )
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
        omop_schema = omop_schema
      )

    output <-
      output %>%
      rubix::filter_at_grepl(concept_name,
        grepl_phrase = ", | and | monotherapy"
      ) %>%
      rubix::arrange_by_nchar(nchar_col = concept_name)

    if (is.null(component_count)) {
      return(output)
    } else if (component_count == 1) {
      output <-
        output %>%
        rubix::filter_at_grepl(concept_name,
          grepl_phrase = " monotherapy"
        ) %>%
        rubix::arrange_by_nchar(nchar_col = concept_name)

      return(output)
    } else if (component_count == 2) {
      output %>%
        rubix::filter_at_grepl(concept_name,
          grepl_phrase = " and "
        ) %>%
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
      max <- 1 + (output %>%
        dplyr::transmute(comma_count = centipede::nchar_comma(concept_name)) %>%
        unlist() %>%
        max(na.rm = TRUE))
      warning('"component_count" max is: ', max, " returning unfiltered output")
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
      rubix::filter_for(
        filter_col = concept_class_id,
        inclusion_vector = c(
          "Regimen",
          "Component"
        ),
        invert = TRUE
      )

    if (nrow(qa)) {
      qaNormalizeToHemOncComponents <<- qa
      stop("input concept ids are not Regimen or Components. See qaNormalizeToHemOncComponents for more details.")
    }

    input_regimens <- input_concept %>%
      dplyr::filter(concept_class_id == "Regimen")

    input_components <- input_concept %>%
      dplyr::filter(concept_class_id == "Component")


    if (nrow(input_regimens)) {
      component_concept_ids <-
        c(
          input_components$concept_id,
          queryHemOncRegToAntineo(
            regimen_concept_ids = input_regimens$concept_id,
            schema = schema
          ) %>%
            dplyr::select(has_antineoplastic_concept_id) %>%
            unlist() %>%
            unname()
        )
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
      dplyr::mutate_at(
        dplyr::vars(contains("concept_id")),
        as.integer
      )
  }



lowLevelQuery <-
  function(conn,
           conn_fun,
           sql_statement,
           verbose = TRUE,
           render_sql = TRUE,
           render_only = FALSE,
           ...) {
    .Deprecated(
      new = "query",
      package = "pg13"
    )
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
      resultset <- DatabaseConnector::dbGetQuery(conn,
        statement = sql_statement,
        ...
      )
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
      on.exit(
        expr = dcAthena(
          conn = conn,
          verbose = verbose
        ),
        add = TRUE,
        after = TRUE
      )
    }

    check_conn(conn = conn)


    if (skip_cache) {
      if (verbose) {
        secretary::typewrite("Skipping cache")
      }

      resultset <- pg13::query(
        conn = conn,
        sql_statement = sql_statement,
        verbose = verbose,
        render_sql = render_sql,
        render_only = render_only
      )
    } else {
      if (override_cache) {
        if (verbose) {
          secretary::typewrite("Overriding cache")
        }

        resultset <- pg13::query(
          conn = conn,
          sql_statement = sql_statement,
          verbose = verbose,
          render_sql = render_sql,
          render_only = render_only
        )


        lowLevelCache(
          data = resultset,
          query = sql_statement
        )
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
            resultset <- pg13::query(
              conn = conn,
              sql_statement = sql_statement,
              verbose = verbose,
              render_sql = render_sql,
              render_only = render_only
            )


            lowLevelCache(
              data = resultset,
              query = sql_statement
            )
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

    fields <- pg13::lsFields(
      conn = conn,
      schema = schema,
      tableName = tableName
    )


    paste0(shortcut, fields, " AS ", prefix, fields, suffix) %>%
      paste(collapse = ",\n") %>%
      cat()
  }



#' @title
#' Query for a Source Vocabulary's Relationships to Other Source Vocabularies
#'
#' @details
#' Query for the Source Vocabulary's non-null relationships in the Concept Relationship Table to a second target set of OMOP Source Vocabulary and its Concept Classes. For a resultset that does not filters on both ends of the relationship, see \code{\link{query_all_vocabulary_relationships}}.
#'
#' @inherit vocabulary_level_functions description
#'
#' @inheritParams queryAthena
#' @param vocabulary_id_1         Vector of 1 or more `vocabulary_id`
#' @param vocabulary_id_2         Vector of 1 or more target `vocabulary_id`. If NULL, the target vocabulary is set to itself. Default: NULL.
#'
#' @seealso
#'  \code{\link[SqlRender]{render}}
#'
#' @rdname query_vocabulary_relationships
#'
#' @export
#' @importFrom SqlRender render

query_vocabulary_relationships <-
  function(vocabulary_id_1,
           vocabulary_id_2 = NULL,
           conn = NULL,
           cache_only = FALSE,
           skip_cache = FALSE,
           override_cache = FALSE,
           cache_resultset = TRUE,
           render_sql = FALSE,
           verbose = FALSE,
           sleepTime = 1) {

    .Deprecated()
    vocabulary_id_1 <- paste0("'", vocabulary_id_1, "'")

    if (is.null(vocabulary_id_2)) {
      vocabulary_id_2 <- vocabulary_id_1
    } else {
      vocabulary_id_2 <- paste0("'", vocabulary_id_2, "'")
    }

    queryAthena(
      sql_statement =
        SqlRender::render(
          "SELECT DISTINCT
                                                        c.vocabulary_id AS vocabulary_id_1,
                                                        c.concept_class_id AS concept_class_id_1,
                                                        cr.relationship_id,
                                                        c2.vocabulary_id AS vocabulary_id_2,
                                                        c2.concept_class_id AS concept_class_id_2
                                                        FROM public.concept c
                                                        LEFT JOIN public.concept_relationship cr
                                                        ON cr.concept_id_1 = c.concept_id
                                                        LEFT JOIN public.concept c2
                                                        ON c2.concept_id = cr.concept_id_2
                                                        WHERE c.vocabulary_id IN (@vocabulary_id_1)
                                                                AND c.invalid_reason IS NULL
                                                                AND c2.vocabulary_id IN (@vocabulary_id_2)
                                                                AND c2.invalid_reason IS NULL
                                                                AND cr.invalid_reason IS NULL",
          vocabulary_id_1 = vocabulary_id_1,
          vocabulary_id_2 = vocabulary_id_2
        ),
      conn = conn,
      cache_only = cache_only,
      skip_cache = skip_cache,
      override_cache = override_cache,
      cache_resultset = cache_resultset,
      render_sql = render_sql,
      verbose = verbose,
      sleepTime = sleepTime
    )
  }

#' @title
#' Check Connection
#' @export
#' @rdname check_conn
#' @importFrom cli cli_alert_success cli_alert_danger

check_conn <-
  function(conn) {
    if (pg13::is_conn_open(conn = conn)) {
      cli::cli_alert_success("Connection is open")
    } else {
      cli::cli_alert_danger("Connection")
      stop("Connection is closed")
    }
  }

#' @title
#' Check Connection Type
#' @export
#' @rdname check_conn_type
#' @importFrom cli cli_alert_success cli_alert_danger

check_conn_type <-
  function(conn) {
    if (!.hasSlot(conn, name = "jConnection")) {
      cli::cli_alert_danger("Connection")
      stop("Connection not JDBC Connection")
    } else {
      cli::cli_alert_success("Connection is JDBC Connection")
    }
  }

lowLevelSend <-
  function(conn,
           conn_fun,
           sql_statement,
           verbose = TRUE,
           render_sql = TRUE,
           render_only = FALSE,
           ...) {
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
        typewrite_activity("Sending...")
      }
      DatabaseConnector::dbSendStatement(
        conn = conn, statement = sql_statement,
        ...
      )
      if (verbose) {
        typewrite_activity("Sending...complete")
      }
    }
  }


#' Find HemOnc Regimen by Components
#' @description This function takes a set list of HemOnc Components and finds all the HemOnc Regimens that contain these Components in the CONCEPT_RELATIONSHIP table. The resultset is then filtered for the HemOnc Regimens that have the same exact number of Components as the length of the `component_concept_ids` parameter.
#' @param component_concept_ids integer vector of length 1 or greater of all the Component Concept Ids belonging to any regimen to map to a HemOnc Regimen.
#' @param ... Additional arguments passed to the query_athena function.
#' @export

queryHemOncCompToReg <-
  function(component_concept_ids,
           schema = NULL,
           conn = NULL,
           cache_only = FALSE,
           skip_cache = FALSE,
           override_cache = FALSE,
           render_sql = FALSE,
           verbose = FALSE,
           sleepTime = 1) {
    .Deprecated("ho_lookup_regimen")

    # For inputs that are actually regimens, a new set of components is derived.
    component_concept_ids <-
      normalizeToHemOncComponents(
        hemonc_concept_ids = component_concept_ids,
        schema = schema
      )

    # Get input component count to filter HemOnc Regimens based on their own component_counts
    input_component_count <- length(component_concept_ids)

    # If any of the concept_ids are regimens, to get their antineoplastic components
    input_concept <-
      queryConceptId(component_concept_ids,
                     schema = schema
      )

    qa <- input_concept %>%
      rubix::filter_for(
        filter_col = concept_class_id,
        inclusion_vector = c(
          "Regimen",
          "Component"
        ),
        invert = TRUE
      )

    if (nrow(qa)) {
      qaHemOncCompToReg <<- qa
      stop("input concept ids are not Regimen or Components. See qaHemOncCompToReg for more details.")
    }


    # Query Athena DB for all Regimens associated with the inputted Component Concept Ids
    sql_statement <-
      renderHemOncCompToReg(
        component_concept_ids = component_concept_ids,
        schema = schema
      )

    Regimens <-
      queryAthena(
        sql_statement = sql_statement,
        conn = conn,
        cache_only = cache_only,
        skip_cache = skip_cache,
        override_cache = override_cache,
        render_sql = render_sql,
        verbose = verbose,
        sleepTime = sleepTime
      )

    # Query again to get all of the "Has antineoplastic" relationships to HemOnc Components these Regimens have
    HasAntineoplastics <- queryHemOncRegToAntineo(
      regimen_concept_ids = Regimens$regimen_concept_id,
      schema = schema
    )



    # Getting the number of unique HemOnc Components associated with each of the HemOnc Regimens found and then filtering for the length of the input component_concept_ids vector
    HasAntineoplastics2 <-
      HasAntineoplastics %>%
      dplyr::group_by(regimen_concept_id) %>%
      dplyr::summarize(has_antineoplastic_count = length(unique(has_antineoplastic_concept_id)), .groups = "drop") %>%
      dplyr::filter(has_antineoplastic_count == input_component_count) %>%
      dplyr::ungroup() %>%
      dplyr::select(regimen_concept_id) %>%
      left_join_concept() %>%
      dplyr::select(-any_of("regimen_concept_id")) %>%
      rubix::rename_all_prefix("regimen_")

    # If only 1 or less rows, the function is complete. Otherwise, the outputs need to be filtered another time since now we have all the Regimens that have the exact component count match as the input and have at least 1 of the input components, but does not necessarily have all the components
    #
    if (nrow(HasAntineoplastics2) <= 1) {
      return(HasAntineoplastics2)
    } else {
      HasAntineoplastics3 <-
        HasAntineoplastics %>%
        dplyr::select(
          regimen_concept_id,
          has_antineoplastic_concept_id
        ) %>%
        dplyr::group_by(regimen_concept_id) %>%
        dplyr::summarise_at(
          vars(has_antineoplastic_concept_id),
          list(
            has_all_components = ~ all(. %in% component_concept_ids),
            component_count = ~ length(unique(.))
          )
        ) %>%
        dplyr::filter(
          has_all_components == TRUE,
          component_count == input_component_count
        ) %>%
        dplyr::select(regimen_concept_id) %>%
        left_join_concept(include_synonyms = F) %>%
        dplyr::select(-starts_with("regimen")) %>%
        rubix::rename_all_prefix("regimen_")

      return(HasAntineoplastics3)
    }
  }




#' Query a HemOnc Regimen's 'Has antineoplastic' Relationship
#' @export

queryHemOncRegToAntineo <-
  function(regimen_concept_ids,
           schema = NULL,
           conn = NULL,
           cache_only = FALSE,
           skip_cache = FALSE,
           override_cache = FALSE,
           render_sql = FALSE,
           verbose = FALSE,
           sleepTime = 1) {
    .Deprecated("ho_lookup_antineoplastics")

    sql_statement <-
      renderHemOncRegToAntineoplastics(
        regimen_concept_ids = regimen_concept_ids,
        schema = schema
      )

    queryAthena(
      sql_statement = sql_statement,
      conn = conn,
      cache_only = cache_only,
      skip_cache = skip_cache,
      override_cache = override_cache,
      render_sql = render_sql,
      verbose = verbose,
      sleepTime = sleepTime
    )
  }




