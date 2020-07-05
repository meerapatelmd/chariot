#' Cache the results of a query
#' @description This function uses the R.cache::saveCache function with the destination directory by Package name "Athena".
#' @return Invisibly returns the path to the cache file
#' @importFrom R.cache saveCache
#' @param object object to cache
#' @param key equivalent to the SQL query string. 
#' @export

load_cached_left_join <-
    function(vector, athena_table, athena_column,where_athena_col,where_athena_col_equals,omop, omop_schema) {
            key <-list(vector=vector, 
                       athena_table=athena_table,
                       athena_column=athena_column,
                       where_athena_col=where_athena_col,
                       where_athena_col_equals=where_athena_col_equals,
                       omop=omop,
                       omop_schema=omop_schema)
            
            
                x <-
                    R.cache::loadCache(key=key,
                                       dirs="athena")
                
            invisible(x)
    }

