.onLoad <-
        function(libname,
                 pkgname) {

                x <-
                tryCatch(
                        read_cdm_wiki_table(),
                        error = function(e) NULL
                )

                if (is.null(x)) {

                        packageStartupMessage(
                                "Read GitHub Wiki <https://ohdsi.github.io/CommonDataModel/cdm60.html>"
                        )

                }


        }
