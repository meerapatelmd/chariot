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

pivotRelationshipId <-
    function(.data,
             column = NULL,
             athena_schema = "public",
             render_sql = TRUE,
             conn = NULL,
             include_count = TRUE) {


                    if (is.null(column)) {

                            column <- colnames(.data)[1]

                    }



                    output <- leftJoinRelationship(.data = .data,
                                                   column = column,
                                                   athena_schema = athena_schema,
                                                   render_sql = render_sql,
                                                   conn = conn) %>%
                                mergeStrip(into = "Concept2",
                                           has_suffix = "_2")



                    final_output <-
                        output %>%
                        tidyr::pivot_wider(id_cols = concept_id_1,
                                       names_from = relationship_id,
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
                                       names_from = relationship_id,
                                       values_from = Concept2,
                                       values_fn = list(Concept2 = function(x) length(unique(x)))) %>%
                    dplyr::rename_at(vars(!(concept_id_1)),
                                     function(x) paste0(x, " Count"))

                final_output <-
                    dplyr::left_join(final_output,
                                     final_output_count,
                                     by = "concept_id_1")


            }

                dplyr::left_join(.data %>%
                                         dplyr::rename(concept_id_1 = !!column),
                                 final_output) %>%
                        dplyr::rename(!!column := concept_id_1)


    }
