#' @export

ids_to_integer <-
        function(data) {

                .Deprecated()

                data %>%
                        dplyr::mutate_at(dplyr::vars(contains("concept_id")),
                                         as.integer)

        }


