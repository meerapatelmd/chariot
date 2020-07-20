#' Disconnect Local Athena
#' @import DatabaseConnector
#' @export

dcAthena <-
    function(conn,
             remove = FALSE) {
        pg13::dc(conn = conn,
                 remove = remove)
    }
