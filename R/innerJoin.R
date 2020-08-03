#' INNER JOIN an OMOP Vocabulary Table
#' @description This function executes the join() function with joinType == "INNER".
#' @export

innerJoin <-
    function(.data,
             column = NULL,
             athena_schema,
             athena_table,
             athena_column,
             where_athena_col = NULL,
             where_athena_col_in = NULL,
             render_sql = TRUE,
             conn = NULL) {


                    join(.data = .data,
                         joinType = "INNER",
                         column = column,
                         athena_schema = athena_schema,
                         athena_table = athena_table,
                         athena_column = athena_column,
                         where_athena_col = where_athena_col_in,
                         where_athena_col_in = where_athena_col_in,
                         render_sql = render_sql,
                         conn = conn)

    }
