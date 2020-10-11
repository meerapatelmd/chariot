
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param conn PARAM_DESCRIPTION, Default: NULL
#' @param sql_statement PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[pg13]{send}}
#' @rdname sendAthena
#' @export
#' @importFrom pg13 send



sendAthena <-
        function(conn = NULL,
                 sql_statement) {


                if (is.null(conn)) {

                        send_conn <- connectAthena()

                } else {

                        send_conn <- conn

                }

                pg13::send(conn = send_conn,
                           sql_statement = sql_statement)


                if (is.null(conn)) {

                        dcAthena(conn = send_conn)

                }


        }
