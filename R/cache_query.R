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

