#' Get all Loinc System (Specimen) Types for a Lab
#' @description This function takes a dataframe and mutates an additional column providing the specimen type based on the "Has system" relationship id.
#' @param concept_id_col The column in dataframe that points to the concept_id. If NULL, defaults to "concept_id".
#' @param dataframe input data
#' @examples
#' Random immunosuppressant concept ids
#' immunosuppressant_concept_ids <- c("35807335","35807331", "21603616", "21600651", "21605199", "21602723")
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr left_join
#' @importFrom dplyr distinct
#' @export

pivotLevel <-
    function(.data,
             column = NULL,
             athena_schema = "public",
             max_levels_of_separation = TRUE,
             AND = TRUE,
             render_sql = TRUE,
             conn = NULL,
             include_count = TRUE) {

            if (is.null(column)) {

                    column <- colnames(.data)[1]

            }

            if (max_levels_of_separation) {
                    levels_type <- "max_levels_of_separation"
            } else {
                    levels_type <- "min_levels_of_separation"
            }


            output <- leftJoinRelatives(.data = .data,
                                        id_column = column,
                                        athena_schema = athena_schema,
                                        render_sql = render_sql,
                                        conn = conn)


            # Split A and D
            output <- split(output,
                            output$relative_type)

            #Arrange descending from A then ascending from 0 to D
            output$A <-
                output$A %>%
                dplyr::arrange_at(vars(!!levels_type),
                                  function(x) desc(as.integer(x))) %>%
                dplyr::mutate_at(vars(!!levels_type),
                                 function(x) paste0("A_", x))


            output$D <-
                output$D %>%
                dplyr::arrange_at(vars(!!levels_type),
                                  function(x) as.integer(x)) %>%
                dplyr::mutate_at(vars(!!levels_type),
                                 function(x) paste0("D_", x))

            # Binding output back
            output <-
                output %>%
                dplyr::bind_rows() %>%
                mergeStrip(into = "RelativeConcept",
                           has_prefix = "relative_") %>%
                    dplyr::select(!!column,
                                  RelativeConcept,
                                  !!levels_type)

            final_output <-
                    output %>%
                        tidyr::pivot_wider(id_cols = !!column,
                                           names_from = !!levels_type,
                                           values_from = RelativeConcept,
                                           values_fn = list(RelativeConcept = function(x) paste(unique(x)[1:250] %>%
                                                                                                       centipede::no_na(), collapse = "\n"))) %>%
                        dplyr::mutate_all(substring, 1, 25000) %>%
                        dplyr::mutate_at(vars(!!column),
                                         as.integer)

            if (include_count) {

                final_output_count <-
                    output %>%
                    tidyr::pivot_wider(id_cols = !!column,
                                       names_from = !!levels_type,
                                       values_from = RelativeConcept,
                                       values_fn = list(RelativeConcept = function(x) length(unique(x)))) %>%
                    dplyr::rename_at(vars(!(!!column)),
                                     function(x) paste0(x, " Count"))

                final_output <-
                    dplyr::left_join(final_output,
                                     final_output_count)



            }

            dplyr::left_join(.data,
                             final_output,
                             by = column)

    }
