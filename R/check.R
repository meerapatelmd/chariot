
#' @export
#' @rdname check_conn
#' @importFrom cli cli_alert_success cli_alert_danger

check_conn <-
        function(conn) {

                if (pg13::is_conn_open(conn = conn)) {

                        cli::cli_alert_success("Connection is open")

                } else {

                        cli::cli_alert_danger("Closed connection")
                }
        }

#' @export
#' @rdname check_concept_id
#' @importFrom cli cli_alert_success cli_alert_danger

check_concept_id <-
        function(concept_id) {

                if (!is.na(concept_id)) {

                        concept_id_int <- suppressWarnings(as.integer(concept_id))

                        if (is.na(concept_id_int)) {

                                cli::cli_alert_danger("Invalid Concept Id")
                        } else {

                                cli::cli_alert_success("Valid Concept Id")
                        }
                } else {

                        cli::cli_alert_danger("Concept Id is NA")
                }
        }
