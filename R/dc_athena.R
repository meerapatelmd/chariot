#' Disconnect Local Athena
#' @import DatabaseConnector
#' @export

dc_athena <-
    function(conn) {
            .Deprecated("dcAthena")
        DatabaseConnector::dbDisconnect(conn)
    }
