#' @title
#' Get Version Key
#' @description
#' The key used as a hash for the caching feature.
#' @param conn PARAM_DESCRIPTION
#' @param conn_fun PARAM_DESCRIPTION, Default: 'pg13::local_connect(verbose=FALSE)'
#' @param schema PARAM_DESCRIPTION, Default: 'omop_vocabulary'
#' @param log_schema PARAM_DESCRIPTION, Default: 'public'
#' @param log_table PARAM_DESCRIPTION, Default: 'setup_athena_log'
#' @param log_timestamp_field PARAM_DESCRIPTION, Default: 'sa_datetime'
#' @param template_only PARAM_DESCRIPTION, Default: FALSE
#' @param sql_only PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details
#' If a `log table` is present as written using the `setupAthena` package,
#' the VersionKey class object is returned using the last row. If the
#' `log_table` is not found, the VersionKey is created from a list of the
#' vocabulary_id-vocabulary_version key-value pair from the Vocabulary table.
#' @rdname get_VersionKey
#' @export
#' @importFrom rlang parse_expr
#' @importFrom pg13 table_exists query
#' @importFrom glue glue
#' @importFrom purrr set_names



get_VersionKey <-
function (conn,
          conn_fun = "pg13::local_connect(verbose=FALSE)",
          schema = "omop_vocabulary",
          log_schema = "public",
          log_table = "setup_athena_log",
          log_timestamp_field = "sa_datetime",
          template_only = FALSE,
          sql_only = FALSE) {

        if (missing(conn)) {

                conn <- eval(rlang::parse_expr(conn_fun))
                on.exit(expr = dcAthena(conn = conn),
                        add = TRUE,
                        after = TRUE)
        }


        read_template <-
                function(file) {

                        file_path <-
                                system.file(package = "chariot",
                                            "version_sql",
                                            file)

                        paste(readLines(con = file_path),
                              collapse = "\n")


                }

        if (
                pg13::table_exists(conn = conn,
                                   schema = log_schema,
                                   table_name = log_table)) {


                sql_template <- read_template(file = "get_logged_version.sql")
                if (template_only) {
                        return(sql_template)
                }
                if (sql_only) {
                        return(glue::glue(sql_template))
                }
                version <- pg13::query(conn = conn, conn_fun = conn_fun,
                                       checks = "", sql_statement = glue::glue(sql_template),
                                       verbose = FALSE, render_sql = FALSE)


                structure(as.list(version),
                          source = sprintf("%s.%s", log_schema, log_table),
                          class = c("chariot_VersionKey",
                                    "list"))

        } else {

        sql_template <- read_template(file = "get_vocabulary_version.sql")
        if (template_only) {
                return(sql_template)
        }
        if (sql_only) {
                return(glue::glue(sql_template))
        }
        version <- pg13::query(conn = conn, conn_fun = conn_fun,
                               checks = "", sql_statement = glue::glue(sql_template),
                               verbose = FALSE, render_sql = FALSE)


        structure(as.list(version$vocabulary_version) %>%
                          purrr::set_names(version$vocabulary_id),
                  source = sprintf("%s.%s", log_schema, log_table),
                  class = c("chariot_VersionKey",
                            "list"))


        }


}


print.chariot_VersionKey <-
        function(x) {

                print.listof(x)

        }
