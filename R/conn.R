#' Connect Local Athena
#' @importFrom pg13 connect
#' @export

connectAthena <-
    function(user = NULL,
             password = NULL,
             port = 5432,
             server = "localhost/athena",
             verbose = TRUE) {

           pg13::connect(user = user,
                         password = password,
                         port = port,
                         server = server,
                         verbose = verbose)

    }


#' Connect Local Athena Function Factory
#' @export

connectAthena_ff <-
        function(user = NULL,
                 password = NULL,
                 port = 5432,
                 server = "localhost/athena") {


                function(verbose = TRUE) {


                        connectAthena(user = user,
                                      password = password,
                                      port = port,
                                      server = server,
                                      verbose = verbose)


                }

        }


#' Disconnect Local Athena
#' @importFrom pg13 dc
#' @export

dcAthena <-
        function(conn,
                 remove = FALSE,
                 verbose = TRUE) {

                pg13::dc(conn = conn,
                         remove = remove,
                         verbose = verbose)
        }


#' Get Connection Database Name
#' @description
#' This is to make sure that the cache path directory has the same name as the database to prevent collisions between multiple OMOP Vocabulary sources used simulataneously.
#' @export



get_conn_db <-
        function(conn) {

                conn@jConnection$getCatalog()
        }
