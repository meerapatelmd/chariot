#' Drop Join Tables
#' @description Drops all tables in the format ("V{timestamp}")
#' @import DatabaseConnector
#' @export

dropJoinTables <-
    function(conn,
             schema = NULL) {

            joinTables  <-  lsJoinTables(conn = conn,
                                         schema = schema)

            for (joinTable in joinTables) {

                pg13::dropTable(conn = conn,
                                schema = schema,
                                tableName = joinTable)

            }

    }
