#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname constraints
#' @export
#' @importFrom SqlRender render



constraints <-
        function(conn,
                 conn_fun = "connectAthena()",
                 vocab_schema,
                 table,
                 field,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 render_sql = TRUE,
                 verbose = TRUE,
                 sleepTime = 1) {


                        if (!missing(conn_fun)) {

                                conn <- eval(expr = rlang::parse_expr(x = conn_fun))
                                on.exit(expr = dcAthena(conn = conn,
                                                        verbose = verbose),
                                        add = TRUE,
                                        after = TRUE)

                        }

                         sql_statement <-
                                 SqlRender::render(
                                         "
                                         SELECT DISTINCT @field
                                         FROM @vocab_schema.@table
                                         ",
                                         field = field,
                                         table = table,
                                         vocab_schema = vocab_schema
                                 )

                         queryAthena(sql_statement = sql_statement,
                                     conn = conn,
                                     cache_only = cache_only,
                                     skip_cache = skip_cache,
                                     override_cache = override_cache,
                                     render_sql = render_sql,
                                     verbose = verbose,
                                     sleepTime = sleepTime) %>%
                                 unlist()


        }
