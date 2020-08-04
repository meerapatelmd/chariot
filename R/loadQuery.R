#' Load a Cached Athena Query
#' @import pg13
#' @export

loadQuery <-
        function(sql_statement,
                 db = NULL) {

                if (is.null(db)) {
                        db <- "athena"
                }

                pg13::loadCachedQuery(sqlQuery = sql_statement,
                                      db = db)

        }
