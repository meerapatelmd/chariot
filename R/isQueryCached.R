#' Is this query cached?
#' @import pg13
#' @export

isQueryCached <-
    function(sql_statement,
             db) {


            cachedData <-
            pg13::loadCachedQuery(sqlQuery = sql_statement,
                                  db = db)

                if (!is.null(cachedData)) {

                        TRUE

                } else {

                    FALSE
                }

    }
