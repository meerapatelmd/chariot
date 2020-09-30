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


#' Load cacheed results of a query
#' @description This function uses the R.cache::loadCache function with the destination directory set to Package name "Athena".
#' @return Cached query resultset
#' @importFrom R.cache loadCache
#' @param key equivalent to the SQL query string.
#' @export

load_cached_query <-
    function(key) {
                key <- list(key)
                x <-
                R.cache::loadCache(key=key,
                                   dirs="athena",
                                   onError="error")
                return(x)
    }


#' Cache the results of a query
#' @description This function uses the R.cache::saveCache function with the destination directory by Package name "Athena".
#' @return Invisibly returns the path to the cache file
#' @importFrom R.cache saveCache
#' @param object object to cache
#' @param key equivalent to the SQL query string.
#' @export

cache_query <-
        function(object, key) {
                key = list(key)
                x <-
                        R.cache::saveCache(object=object,
                                           key=key,
                                           dirs="athena")
                invisible(x)
        }
