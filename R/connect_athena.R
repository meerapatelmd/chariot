#' Connect Local Athena
#' @import pg13
#' @export

connect_athena <-
    function() {

            pg13::localConnect(dbname = "athena",
                               port = 5432)

    }
