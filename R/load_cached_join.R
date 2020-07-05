#' Cache the results of a query
#' @description This function uses the R.cache::saveCache function with the destination directory by Package name "Athena".
#' @return Invisibly returns the path to the cache file
#' @importFrom R.cache saveCache
#' @param object object to cache
#' @param key equivalent to the SQL query string. 
#' @export

load_cached_join <-
    function(function_name,...) {
            key <- list(function_name,...)
            
            print(key)
                x <-
                    R.cache::loadCache(key=key,
                                       dirs="chariot")
                
            invisible(x)
    }

