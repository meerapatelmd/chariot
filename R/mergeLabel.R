#' Merge OMOP Concepts into a Strip
#' @description All elements of the CONCEPT table are included except for the dates. While this function and unmerge_concepts are meant to be the inverse of one another, it is important to note that this function will select for only the columns in the input that match the pattern of {has_prefix}column_name{has_suffix} and therefore may need to be rejoined to the input after completion. The inverse unmerge_concepts will not remove any columns in the output. It uses the tidyr::extract function to operate directly on the provided column.
#' @param .data output from concept table
#' @param into name of the column that the new combined string will be in
#' @param ... columns other than concept_id that will be removed in tidyr unite but should be preserved in addition to be merged.
#' @param has_suffix if the omop concept element column names are different from the standard by a suffix, include it so it can point to the correct set of columns
#' @param has_prefix if the omop concept element column names are prefixed, include it so it can point to the correct set of columns
#' @param label if TRUE, only have a strip in the format {concept_id} {concept_name}
#' @param keep_other_cols TRUE if all the non-concept table columns in the input dataframe is desired in the output
#' @param shorthand This only returns the validity, standard, concept_id, and concept_name as a string. Please note that this merge cannot be unmerged using the unmerge_concepts function.
#' @import dplyr
#' @import tidyr
#' @export

mergeStrip <-
            function(.data,
                     into,
                     ...,
                     has_suffix = NULL,
                     has_prefix = NULL,
                     label = FALSE,
                     keep_other_cols = TRUE,
                     shorthand = FALSE) {


                                # Enquo output column name
                                into <- dplyr::enquo(into)

                                # Preserve columns
                                preserve_cols <- dplyr::enquos(...)

                                # Generating a list of concept table columns that includes prefixes and suffixes
                                column_names <- paste0(has_prefix,
                                                        c("concept_id",
                                                         "concept_name",
                                                         "domain_id",
                                                         "vocabulary_id",
                                                         "concept_class_id",
                                                         "standard_concept",
                                                         "concept_code",
                                                         "valid_start_date",
                                                         "valid_end_date",
                                                         "invalid_reason"),
                                                       has_suffix)


                                if (!(all(column_names %in% colnames(.data)))) {

                                        qa <- column_names[!(column_names %in% colnames(.data))]
                                        stop("missing columns: ", qa)

                                }

                if (label == FALSE) {
                                output <-
                                .data %>%
                                        dplyr::select(any_of(column_names)) %>%
                                        dplyr::mutate_at(vars(contains("standard_concept")), function(x) ifelse(is.na(x), "N", x)) %>%
                                        dplyr::mutate_at(vars(contains("standard_concept")), function(x) paste0("[", x, "]")) %>%
                                        dplyr::mutate_at(vars(contains("invalid_reason")), function(x) ifelse(is.na(x), "[V]", paste0("[", x, "]"))) %>%
                                        tidyr::unite(col = vocabulary, contains("vocabulary_id"), contains("concept_code"), sep = " ") %>%
                                        dplyr::mutate_at(vars(contains("domain_id"),
                                                              contains("vocabulary"),
                                                              contains("concept_class_id")), function(x) paste0("[", x, "]")) %>%
                                        dplyr::select_at(vars(!matches("valid.*date"))) %>%
                                        tidyr::unite(col = !!into,
                                                     contains("invalid_reason"),
                                                     contains("standard_concept"),
                                                     contains("concept_id"),
                                                     contains("concept_name"),
                                                     contains("vocabulary"),
                                                     contains("domain_id"),
                                                     contains("concept_class_id"),
                                                     sep = " ",
                                                     remove = TRUE)

                } else {


                                output <-
                                            .data %>%
                                                            dplyr::select(any_of(column_names)) %>%
                                                             dplyr::select(contains("concept_id"),
                                                                           contains("concept_name")) %>%
                                                             tidyr::unite(col = !!into,
                                                                          contains("concept_id"),
                                                                          contains("concept_name"),
                                                                          sep = " ",
                                                                          remove = TRUE)

                }

                                add_back_concept_id_col <- paste0(has_prefix, "concept_id", has_suffix)

                                output <- dplyr::bind_cols(.data %>%
                                                                  dplyr::select(all_of(add_back_concept_id_col)),
                                                              output)

                                # All NA concepts are not merged into a strip and returns a single NA
                                output <-
                                    output %>%
                                    dplyr::mutate_at(vars(!!into),
                                                     function(x) ifelse(grepl("NA NA \\[NA NA\\] \\[NA\\] \\[NA\\]",
                                                                              x,
                                                                              ignore.case = FALSE),
                                                                        NA_character_,
                                                                        x))


                                if (!missing(...)) {

                                        output <-
                                                dplyr::bind_cols(output,
                                                                 .data %>%
                                                                     dplyr::select(!!!preserve_cols))

                                }

                                if (keep_other_cols == TRUE)  {

                                            output <-
                                                bind_cols(output,
                                                          .data %>%
                                                            dplyr::select(-(any_of(column_names))))


                                }

                                return(output)

            }

