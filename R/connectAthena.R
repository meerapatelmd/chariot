#' Connect Local Athena
#' @import pg13
#' @export

connectAthena <-
    function() {

            pg13::localConnect(dbname = "athena",
                               port = 5432)

    }
