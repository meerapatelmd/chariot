#' Connect Local Athena
#' @import pg13
#' @export

connectAthena <-
    function() {

            pg13::localConnect(dbname = "athena",
                               port = 5432)

    }


#' Disconnect Local Athena
#' @import DatabaseConnector
#' @export

dcAthena <-
        function(conn,
                 remove = FALSE) {
                pg13::dc(conn = conn,
                         remove = remove)
        }
