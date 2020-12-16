#' @title
#' Check Connection
#' @export
#' @rdname check_conn
#' @importFrom cli cli_alert_success cli_alert_danger

check_conn <-
        function(conn) {

                if (pg13::is_conn_open(conn = conn)) {

                        cli::cli_alert_success("Connection is open")

                } else {

                        cli::cli_alert_danger("Connection")
                        stop("Connection is closed")
                }
        }

#' @title
#' Check Connection Type
#' @export
#' @rdname check_conn_type
#' @importFrom cli cli_alert_success cli_alert_danger

check_conn_type <-
        function(conn) {

                if (!.hasSlot(conn, name = "jConnection")) {

                        cli::cli_alert_danger("Connection")
                        stop("Connection not JDBC Connection")

                } else {

                        cli::cli_alert_success("Connection is JDBC Connection")
                }

        }

#' @title
#' Check Concept Id
#' @export
#' @rdname check_concept_id
#' @importFrom cli cli_alert_success cli_alert_danger

check_concept_id <-
        function(concept_id) {

                if (!is.na(concept_id)) {

                        concept_id_int <- suppressWarnings(as.integer(concept_id))

                        if (is.na(concept_id_int)) {

                                cli::cli_alert_danger("Concept Id")
                                stop("Concept Id is not a valid integer")

                        } else {

                                cli::cli_alert_success("Valid Concept Id")
                        }
                } else {

                        cli::cli_alert_danger("Concept Id")
                        stop("Concept Id is NA")
                }
        }





check_vocab_class <-
        function(conn,
                 concept_obj,
                 vocab_schema,
                 vocabulary_id,
                 concept_class_id,
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 render_sql = FALSE,
                 verbose = FALSE,
                 sleepTime = 1) {


                if (class(concept_obj) == "concept") {

                        concept_id <- concept_obj@concept_id
                        check_concept_id(concept_id = concept_id)

                        if (!identical(vocabulary_id, concept_obj@vocabulary_id)|
                            !identical(concept_class_id, concept_obj@concept_class_id)) {

                                cli::cli_alert_danger("Vocabulary and Concept Class Id")
                                stop(sprintf("Vocabulary Id and Concept Class Id are not %s and %s", vocabulary_id, concept_class_id))

                        } else {

                                cli::cli_alert_success("Vocabulary and Concept Class Id")


                        }


                } else {

                        concept_id <- concept_obj
                        check_concept_id(concept_id = concept_id)
                        output <-
                        lookup_concept_id(concept_id = concept_id,
                                          vocab_schema = vocab_schema,
                                          conn = conn,
                                          cache_only = cache_only,
                                          skip_cache = skip_cache,
                                          override_cache = override_cache,
                                          render_sql = render_sql,
                                          verbose = verbose,
                                          sleepTime = sleepTime)

                        if (!identical(vocabulary_id, output$vocabulary_id)|
                            !identical(concept_class_id, output$concept_class_id)) {

                                cli::cli_alert_danger("Vocabulary and Concept Class Id")
                                stop(sprintf("Vocabulary Id and Concept Class Id are not %s and %s", vocabulary_id, concept_class_id))

                        } else {

                                cli::cli_alert_success("Vocabulary and Concept Class Id")


                        }


                }


        }
