#' @title
#' Connect Local Athena
#' @importFrom pg13 local_connect
#' @export
#' @rdname connectAthena

connectAthena <-
  function(port    = 5432,
           dbname  = "athena",
           verbose = FALSE) {
    pg13::local_connect(
      dbname  = dbname,
      port    = port,
      verbose = verbose
    )
  }


#' @title
#' Connect Local Athena Function Factory
#' @export
#' @rdname connectAthena_ff

connectAthena_ff <-
  function(user = NULL,
           password = NULL,
           port = 5432,
           server = "localhost/athena") {
    function(verbose = TRUE) {
      connectAthena(
        user = user,
        password = password,
        port = port,
        server = server,
        verbose = verbose
      )
    }
  }


#' @title
#' Disconnect Local Athena
#' @importFrom pg13 dc
#' @export
#' @rdname dcAthena

dcAthena <-
  function(conn,
           remove =  FALSE,
           verbose = FALSE) {
    pg13::dc(
      conn    = conn,
      remove  = remove,
      verbose = verbose
    )
  }
