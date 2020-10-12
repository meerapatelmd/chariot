
#' @title
#' Send a SQL Statement
#' @description
#' Unlike the \code{\link{queryAthena}} function, the render_sql parameter provides a pause after rendering in case the user wants to copy and paste the rendered SQL into a client if the session is interactive. This is particularly useful with large operations that are better executed within a background client.
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
                 sql_statement,
                 render_sql = FALSE) {


                if (is.null(conn)) {

                        send_conn <- connectAthena()

                } else {

                        send_conn <- conn

                }

                if (interactive()) {
                        if (render_sql) {
                                secretary::typewrite(sql_statement)
                                secretary::press_enter()
                        }
                }

                pg13::send(conn = send_conn,
                           sql_statement = sql_statement)


                if (is.null(conn)) {

                        dcAthena(conn = send_conn)

                }


        }
