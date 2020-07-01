#' Connect Local Athena
#' @import seagull
#' @export

connect_athena <-
    function() {
        seagull::connect_to_local_postgres(dbname = "athena")
    }
