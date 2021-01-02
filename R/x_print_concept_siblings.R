#' @title
#' Print a Concept's Parents and Siblings
#' @description
#' Print all of a concept's parents and siblings.
#' @inheritParams print_concept_hierarchy
#' @seealso
#'  \code{\link[rlang]{parse_expr}}
#'  \code{\link[secretary]{character(0)}}
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{select}}
#'  \code{\link[rubix]{split_deselect}}
#'  \code{\link[purrr]{map}}
#' @rdname print_concept_siblings
#' @export
#' @importFrom rlang parse_expr
#' @importFrom secretary enbold
#' @importFrom tibble tibble
#' @importFrom dplyr filter select
#' @importFrom rubix split_deselect
#' @importFrom purrr map
#' @example inst/example/print_concept_siblings.R

print_concept_siblings <-
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


                if (level_of_separation_type == "min") {
                parents <-
                        join_for_ancestors(data = data,
                                           descendant_id_column = "concept_hierarchy_id",
                                           write_schema = write_schema,
                                           vocab_schema = vocab_schema,
                                           conn = conn,
                                           verbose = verbose,
                                           render_sql = render_sql,
                                           render_only = render_only) %>%
                        dplyr::filter(min_levels_of_separation == 1) %>%
                        merge_strip(into = "parent",
                                    prefix = "ancestor_") %>%
                        dplyr::select(parent_id, parent)


                children <-
                        join_for_descendants(data = parents,
                                             ancestor_id_column = "parent_id",
                                             write_schema = write_schema,
                                             vocab_schema = vocab_schema,
                                             conn = conn,
                                             verbose = verbose,
                                             render_sql = render_sql,
                                             render_only = render_only) %>%
                        dplyr::filter(min_levels_of_separation == 1) %>%
                        merge_strip(into = "children",
                                    prefix = "descendant_") %>%
                        dplyr::select(parent,
                                      children) %>%
                        rubix::split_deselect(col = parent) %>%
                        purrr::map(unlist) %>%
                        purrr::map(function(x) c(target_concept, x)) %>%
                        purrr::map(~ sprintf("\t%s\n", .))


                for (i in seq_along(children)) {
                        cat(names(children)[i], sep = "\n")
                        cat(children[[i]])
                }

                } else {

                        parents <-
                                join_for_ancestors(data = data,
                                                   descendant_id_column = "concept_hierarchy_id",
                                                   write_schema = write_schema,
                                                   vocab_schema = vocab_schema,
                                                   conn = conn,
                                                   verbose = verbose,
                                                   render_sql = render_sql,
                                                   render_only = render_only) %>%
                                dplyr::filter(max_levels_of_separation == 1) %>%
                                merge_strip(into = "parent",
                                            prefix = "ancestor_") %>%
                                dplyr::select(parent_id, parent)


                        children <-
                        join_for_descendants(data = parents,
                                             ancestor_id_column = "parent_id",
                                             write_schema = write_schema,
                                             vocab_schema = vocab_schema,
                                             conn = conn,
                                             verbose = verbose,
                                             render_sql = render_sql,
                                             render_only = render_only) %>%
                                dplyr::filter(max_levels_of_separation == 1) %>%
                                merge_strip(into = "children",
                                            prefix = "descendant_") %>%
                                dplyr::select(parent,
                                              children) %>%
                                rubix::split_deselect(col = parent) %>%
                                purrr::map(unlist) %>%
                                purrr::map(function(x) c(target_concept, x)) %>%
                                purrr::map(~ sprintf("\t%s\n", .))


                        for (i in seq_along(children)) {
                                cat(names(children)[i], sep = "\n")
                                cat(children[[i]])
                        }



                }
        }
