#' List Join Tables
#' @import pg13
#' @export

lsJoinTables <-
        function(conn,
                 schema = NULL) {
                Tables <- pg13::lsTables(conn = conn,
                                         schema = schema)

                grep("^V[0-9]{14}$", Tables, value = TRUE, ignore.case = TRUE)


        }
