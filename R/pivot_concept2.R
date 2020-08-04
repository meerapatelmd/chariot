#' Get all Loinc System (Specimen) Types for a Lab
#' @description This function takes a dataframe and mutates an additional column providing the specimen type based on the "Has system" relationship id.
#' @param concept_id_col The column in dataframe that points to the concept_id. If NULL, defaults to "concept_id".
#' @param dataframe input data
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr left_join
#' @importFrom dplyr distinct
#' @export

pivot_concept2 <-
    function(.data,
             column = NULL,
             names_from,
             include_count = TRUE,
             omop = FALSE,
             omop_schema = "omop_vocabulary") {


        .Deprecated()


        if (missing(names_from)) {

            stop('argument "names_from" is missing, with no default')

        }

            names_from <- paste0(names_from, "_2")

            output <- left_join_relationship(.data = .data,
                                             column = column,
                                             merge_concept2 = FALSE,
                                             omop = omop,
                                             omop_schema = omop_schema) %>%
                        merge_concepts(into = Concept2,
                                       !!names_from,
                                       suffix = "_2")


            final_output <-
                output %>%
                tidyr::pivot_wider(id_cols = concept_id_1,
                               names_from = !!names_from,
                               values_from = Concept2,
                               values_fn = list(Concept2 = function(x) paste(unique(x)[1:250] %>%
                                                                                  centipede::no_na(),
                                                                              collapse = "\n"))) %>%
                dplyr::mutate_all(substring, 1, 25000) %>%
                dplyr::mutate_at(vars(concept_id_1),
                                 as.integer)


            if (include_count) {

                final_output_count <-
                    output %>%
                    tidyr::pivot_wider(id_cols = concept_id_1,
                                       names_from = !!names_from,
                                       values_from = Concept2,
                                       values_fn = list(Concept2 = function(x) length(unique(x)))) %>%
                    dplyr::rename_at(vars(!(concept_id_1)),
                                     function(x) paste0(x, " Count"))

                final_output <-
                    dplyr::left_join(final_output,
                                     final_output_count,
                                     by = "concept_id_1")

                return(final_output)

            } else {
                return(final_output)
            }


    }
