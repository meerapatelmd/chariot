lowLevelQuery <-
        function (conn,
                  conn_fun,
                  sql_statement,
                  verbose = TRUE,
                  render_sql = TRUE,
                  warn_no_rows = TRUE,
                  render_only = FALSE,
                  ...)

                {
                        if (render_only) {
                                typewrite_sql(sql_statement = sql_statement)
                        }
                        else {
                                if (!missing(conn_fun)) {
                                        conn <- eval(rlang::parse_expr(conn_fun))
                                        on.exit(dc(conn = conn))
                                }
                                cli::cat_rule("Checks")
                                check_conn(conn = conn)
                                if (render_sql) {
                                        typewrite_sql(sql_statement = sql_statement)
                                }
                                if (warn_no_rows) {
                                        on.exit(flag_no_rows(data = resultset), add = TRUE,
                                                after = TRUE)
                                }
                                if (verbose) {
                                        typewrite_activity("Querying...")
                                }
                                resultset <- DatabaseConnector::dbGetQuery(conn, statement = sql_statement,
                                                                           ...)
                                if (verbose) {
                                        typewrite_activity("Querying...complete")
                                }
                                resultset
                        }
        }


lowLevelCache <-
        function (data, query)
        {
                R.cache::saveCache(object = data, key = list(query),
                                   dirs = "athena")
        }


lowLevelLoadCache <-
        function(query) {
                R.cache::loadCache(key = list(query), dirs = "athena")
        }



lowLevelSend <-
        function (conn,
                  conn_fun,
                  sql_statement,
                  verbose = TRUE,
                  render_sql = TRUE,
                  render_only = FALSE,
                  ...)
        {
                if (render_only) {
                        typewrite_sql(sql_statement = sql_statement)
                }
                else {
                        if (!missing(conn_fun)) {
                                conn <- eval(rlang::parse_expr(conn_fun))
                                on.exit(dc(conn = conn))
                        }
                        cli::cat_rule("Checks")
                        check_conn(conn = conn)
                        if (render_sql) {
                                typewrite_sql(sql_statement = sql_statement)
                        }
                        if (verbose) {
                                typewrite_activity("Sending...")
                        }
                        DatabaseConnector::dbSendStatement(conn = conn, statement = sql_statement,
                                                           ...)
                        if (verbose) {
                                typewrite_activity("Sending...complete")
                        }
                }
        }
