#' @title
#' Print a Concept's Hierarchy to the Console
#' @description
#' If a level of the hierarchy has more than 10 concepts, the first 10 are
#' printed with an ellipses is included to indicate only the first 10 rows are
#' shown.
#' @param concept_obj  Concept class object or a concept id.
#' @seealso
#'  \code{\link[secretary]{character(0)}}
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[rubix]{split_by}}
#'  \code{\link[purrr]{map}}
#'  \code{\link[dplyr]{arrange}}
#' @rdname print_concept_hierarchy
#' @export
#' @importFrom secretary enbold
#' @importFrom tibble tibble
#' @importFrom rubix split_by
#' @importFrom purrr map
#' @importFrom dplyr arrange
#' @example inst/example/print_concept_hierarchy.R
print_concept_hierarchy <-
        function(concept_obj,
                 level_of_separation_type = c("max", "min"),
                 write_schema = "patelm9",
                 vocab_schema = "omop_vocabulary",
                 conn,
                 conn_fun = "connectAthena()",
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE) {

                if (missing(conn)) {

                        conn <- eval(rlang::parse_expr(conn_fun))
                        on.exit(expr = dcAthena(conn = conn),
                                add = TRUE,
                                after = TRUE)
                }

                level_of_separation_type <-
                        match.arg(arg = level_of_separation_type,
                                  choices = c("max", "min"),
                                  several.ok = FALSE)

                if (class(concept_obj) == "concept") {
                  concept_id <- concept_obj@concept_id
                } else {
                   concept_id <- concept_obj
                }

                target_concept <- get_strip(concept_id = concept_id,
                                            vocab_schema = vocab_schema,
                                            conn = conn,
                                            cache_only = cache_only,
                                            skip_cache = skip_cache,
                                            override_cache = override_cache,
                                            render_sql = render_sql,
                                            verbose = verbose)

                target_concept <- secretary::enbold(sprintf("*%s", target_concept))

                data <- tibble::tibble(concept_hierarchy_id = concept_id)
                if (level_of_separation_type %in% "min") {

                ancestors <-
                        join_for_ancestors(data = data,
                                           descendant_id_column = "concept_hierarchy_id",
                                           write_schema = write_schema,
                                           vocab_schema = vocab_schema,
                                           conn = conn,
                                           verbose = verbose,
                                           render_sql = render_sql,
                                           render_only = render_only) %>%
                        merge_strip(into = "ancestor",
                                    prefix = "ancestor_") %>%
                        rubix::split_by(col = min_levels_of_separation) %>%
                        rev() %>%
                        purrr::map(function(x) x %>%
                                           select(ancestor) %>%
                                           unlist() %>%
                                           unname())

                ancestors[[length(ancestors)+1]] <- target_concept


                descendants <-
                        join_for_descendants(
                                data = data,
                                ancestor_id_column = "concept_hierarchy_id",
                                write_schema = write_schema,
                                vocab_schema = vocab_schema,
                                conn = conn,
                                verbose = verbose,
                                render_sql = render_sql,
                                render_only = render_only
                        ) %>%
                        merge_strip(into = "descendant",
                                    prefix = "descendant_") %>%
                        dplyr::arrange(min_levels_of_separation) %>%
                        rubix::split_by(col = min_levels_of_separation) %>%
                        purrr::map(function(x) x %>%
                                           select(descendant) %>%
                                           unlist() %>%
                                           unname())

                } else {

                        ancestors <-
                                join_for_ancestors(data = data,
                                                   descendant_id_column = "concept_hierarchy_id",
                                                   write_schema = write_schema,
                                                   vocab_schema = vocab_schema,
                                                   conn = conn,
                                                   verbose = verbose,
                                                   render_sql = render_sql,
                                                   render_only = render_only) %>%
                                merge_strip(into = "ancestor",
                                            prefix = "ancestor_") %>%
                                rubix::split_by(col = max_levels_of_separation) %>%
                                rev() %>%
                                purrr::map(function(x) x %>%
                                                   select(ancestor) %>%
                                                   unlist() %>%
                                                   unname())

                        ancestors[[length(ancestors)+1]] <- target_concept


                        descendants <-
                                join_for_descendants(
                                        data = data,
                                        ancestor_id_column = "concept_hierarchy_id",
                                        write_schema = write_schema,
                                        vocab_schema = vocab_schema,
                                        conn = conn,
                                        verbose = verbose,
                                        render_sql = render_sql,
                                        render_only = render_only
                                ) %>%
                                merge_strip(into = "descendant",
                                            prefix = "descendant_") %>%
                                dplyr::arrange(min_levels_of_separation) %>%
                                rubix::split_by(col = max_levels_of_separation) %>%
                                purrr::map(function(x) x %>%
                                                   select(descendant) %>%
                                                   unlist() %>%
                                                   unname())

                }

                # Removed level 0 because can have >900 concepts at this level
                ancestors$`0` <- NULL

                # Removed level 0 because can have >900 concepts at this level
                descendants$`0` <- NULL

                for (i in seq_along(ancestors)) {

                        if (length(ancestors[[i]]) <= 10) {
                                cat(sprintf("%s%s\n", paste(rep("\t", i), collapse = ""), ancestors[[i]]))

                        } else {
                                cat(sprintf("%s%s\n", paste(rep("\t", i), collapse = ""), c(ancestors[[i]][1:10], "...")))
                        }
                }

                for (j in seq_along(descendants)) {

                        if (length(descendants[[j]]) <= 10) {
                        cat(sprintf("%s%s\n", paste(rep("\t", i+j), collapse = ""), descendants[[j]]))
                        } else {
                                cat(sprintf("%s%s\n", paste(rep("\t", i+j), collapse = ""), c(descendants[[j]][1:10], "...")))
                        }
                }

        }



