#' LEFT JOIN the Concept Parent Table
#' @export

leftJoinFoChildren <-
        function(.data,
                 athena_schema,
                 parent_id_column = NULL,
                 render_sql = TRUE,
                 conn = NULL) {


                leftJoin(.data = .data,
                         column = parent_id_column,
                         athena_schema = athena_schema,
                         athena_table = "concept_parent",
                         athena_column = "parent_concept_id",
                         render_sql = render_sql,
                         conn = conn) %>%
                        dplyr::filter(parent_concept_id != child_concept_id)


        }
