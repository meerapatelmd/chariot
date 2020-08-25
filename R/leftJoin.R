#' LEFT JOIN an OMOP Vocabulary Table
#' @description
#' This function executes the join() function with joinType == "LEFT".
#' @export

leftJoin <-
    function(.data,
             column = NULL,
             athena_schema,
             athena_table,
             athena_column,
             where_athena_col = NULL,
             where_athena_col_in = NULL,
             verbose = FALSE,
             conn = NULL,
             render_sql = FALSE,
             sleepTime = 1) {



                    join(.data = .data,
                         joinType = "LEFT",
                         column = column,
                         athena_schema = athena_schema,
                         athena_table = athena_table,
                         athena_column = athena_column,
                         where_athena_col = where_athena_col,
                         where_athena_col_in = where_athena_col_in,
                         verbose = verbose,
                         conn = conn,
                         render_sql = render_sql,
                         sleepTime = sleepTime)

    }
