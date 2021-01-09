.onLoad <-
        function(libname,
                 pkgname) {

                packageStartupMessage(
                        "Reading GitHub Wiki..."
                )
                x <-
                tryCatch(
                        read_cdm_wiki_table(),
                        error = function(e) NULL
                )

                packageStartupMessage(
                        "Reading GitHub Wiki...complete"
                )


        }
