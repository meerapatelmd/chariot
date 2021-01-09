.onLoad <-
        function(libname,
                 pkgname) {

                tryCatch(
                        read_cdm_wiki_table(),
                        error = function(e) NULL
                )

        }
