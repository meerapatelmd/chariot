#' @importFrom secretary typewrite greenTxt
#' @importFrom stringr str_replace_all
#' @noRd

typewrite_sql <-
        function(sql_statement) {
                sql_statement <- stringr::str_replace_all(
                        sql_statement,
                        "[\r\n\t]{1,}|\\s{2,}", " "
                )
                sql_statement <- trimws(sql_statement)
                secretary::typewrite(secretary::greenTxt("SQL:"), sql_statement)
        }

#' Typewrite Activity
#' @importFrom secretary typewrite yellowTxt
#' @noRd

typewrite_activity <-
        function(activity) {
                secretary::typewrite(secretary::yellowTxt(activity))
        }
