


makeSelectFields <-
        function(shortcut = NULL,
                  schema,
                  tableName,
                  prefix = NULL,
                  suffix = NULL,
                 conn) {

                if (is.null(prefix) && is.null(suffix)) {

                        stop("At least one prefix or one suffix must be supplied.")

                }

                if (!is.null(shortcut)) {
                        shortcut <- sprintf("%s.", shortcut)
                }

                fields <- pg13::lsFields(conn = conn,
                                         schema = schema,
                                         tableName = tableName)


                paste0(shortcut, fields, " AS ", prefix, fields, suffix) %>%
                        paste(collapse = ",\n") %>%
                        cat()

        }
