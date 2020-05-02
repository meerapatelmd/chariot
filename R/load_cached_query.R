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
