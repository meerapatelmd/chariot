#' Disconnect Local Athena
#' @import DatabaseConnector
#' @export

dc_athena <-
    function(conn) {
        DatabaseConnector::dbDisconnect(conn)
    }
