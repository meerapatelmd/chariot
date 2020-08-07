




separateConceptStrip <-
        function(.data,
                 ...) {

                tidyr::separate_rows(.data,
                                     ...,
                                     sep = "(?<=\\])\n(?=\\[A-Z\\])")
        }
