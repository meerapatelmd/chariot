#' Cache the results of a query
#' @description This function uses the R.cache::saveCache function with the destination directory by Package name "Athena".
#' @return Invisibly returns the path to the cache file
#' @importFrom R.cache saveCache
#' @param object object to cache
#' @param key equivalent to the SQL query string. 
#' @export

load_cached_join <-
    function(function_name, left_vector, right_table_name, right_column_name, ...) {
            key = list(function_name,
                       left_vector,
                       right_table_name,
                       right_column_name,
                       ...)
            x <-
                R.cache::loadCache(key=key,
                                   dirs="athena")
            invisible(x)
    }

