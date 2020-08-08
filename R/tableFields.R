#' @title List Table Fields
#' @param schema Schema of the Vocabulary Table
#' @param tableName Vocabulary Table
#' @param conn Connection object. If NULL, automatically queries the local Athena database. Default: NULL
#' @return
#' Vector of field names for the table.
#' @seealso
#'  \code{\link[pg13]{lsFields}}
#' @rdname .tableFields
#' @export
#' @importFrom pg13 lsFields




.tableFields <-
        function(schema,
                 tableName,
                 conn = NULL) {

                if (is.null(conn)) {
                        conn <- connectAthena()

                        resultset <-
                        pg13::lsFields(conn = conn,
                                                   tableName = tableName,
                                                   schema = schema)

                        dcAthena(conn = conn)

                } else {
                        resultset <-
                        pg13::lsFields(conn = conn,
                                                   tableName = tableName,
                                                   schema = schema)
                }

                return(resultset)
        }
