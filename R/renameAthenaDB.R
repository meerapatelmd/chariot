#' Rename the current Athena DB
#' @import pg13
#' @param local_dbname Name of a local database other than Athena to connect to to execute the rename SQL statements.
#' @export

renameAthenaDB <-
    function(local_dbname) {

            new_db_name <- paste0("athena", rubix::dated(punct = TRUE))

            conn <- pg13::localConnect(dbname = nonathena_dbname)

            pg13::renameDB(conn = conn,
                           db = "athena",
                           newDB = new_db_name)

            pg13::dc(conn = conn)

    }
