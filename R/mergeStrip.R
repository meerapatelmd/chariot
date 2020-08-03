#' Merge OMOP Concepts into a Strip
#' @description This function takes a set of the OMOP Vocabulary Concept Table fields and merges all of them except for the date fields into a single Concept "Strip". If the Strip output is `<NA>` while the input concept id is not, a flagMergeStrip object is returned in the Global Environment.
#' @return A tibble with all blank and "NA" normalized to `<NA>` with 1. If present, `valid_start_date` and `valid_end_date` fields are permanently removed, 2. 8 out of the 10 remaining Concept Table fields (concept_id, concept_name, domain_id, vocabulary_id, concept_class_id, standard_concept, concept_code, invalid_reason) are merged into a single column with the provided column name, 3. the concept_id column is renamed to the format of the provided merged column name: {into_}concept_id. The remaining of the 7 Concept Table fields may also be preserved outside of the merge if provided. All other columns present in the input data are returned along with the transformations described.
#' @param .data dataframe with the following required fields from the output
#' @param into name of the column that the new combined string will be in
#' @param ... columns other than concept_id that will be removed in tidyr unite but should be preserved in addition to be merged.
#' @param has_suffix if the omop concept element column names are different from the standard by a suffix, include it so it can point to the correct set of columns
#' @param has_prefix if the omop concept element column names are prefixed, include it so it can point to the correct set of columns
#' @import dplyr
#' @import tidyr
#' @export

mergeStrip <-
            function(.data,
                     into,
                     ...,
                     has_suffix = NULL,
                     has_prefix = NULL) {


                                into_id_colname <- paste0(into, "_concept_id")

                                # Enquo output column name
                                into <- dplyr::enquo(into)


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
                                                       has_suffix) %>%
                                                as.list()

                                names(column_names) <-  c("concept_id",
                                                          "concept_name",
                                                          "domain_id",
                                                          "vocabulary_id",
                                                          "concept_class_id",
                                                          "standard_concept",
                                                          "concept_code",
                                                          "valid_start_date",
                                                          "valid_end_date",
                                                          "invalid_reason")



                                # Preserve columns
                                preserve_cols <- dplyr::enquos(...)


                                if (!(all(unlist(column_names) %in% colnames(.data)))) {

                                        qa <- unlist(column_names)[!(unlist(column_names) %in% colnames(.data))]
                                        qa <- grep(pattern = "valid_start_date|valid_end_date",
                                                   qa,
                                                   value = TRUE,
                                                   invert = TRUE)

                                        if (length(qa)) {
                                                stop("missing columns: ", qa)
                                        }

                                }

                                # All other column names
                                other_cols <<- colnames(.data)[!(colnames(.data) %in% unlist(column_names))]


                                output <-
                                .data %>%
                                        dplyr::mutate_at(vars(all_of(column_names$standard_concept)), function(x) ifelse(is.na(x), "N", x)) %>%
                                        dplyr::mutate_at(vars(all_of(column_names$standard_concept)), function(x) paste0("[", x, "]")) %>%
                                        dplyr::mutate_at(vars(all_of(column_names$invalid_reason)), function(x) ifelse(is.na(x), "[V]", paste0("[", x, "]"))) %>%
                                        tidyr::unite(col = vocabulary,
                                                     all_of(c(column_names$vocabulary_id,
                                                              column_names$concept_code)),
                                                     sep = " ") %>%
                                        dplyr::mutate_at(vars(all_of(c(column_names$domain_id,
                                                                       "vocabulary",
                                                                       column_names$concept_class_id))),
                                                         function(x) paste0("[", x, "]")) %>%
                                        #dplyr::select_at(vars(!matches("valid.*date"))) %>%
                                        tidyr::unite(col = !!into,
                                                     all_of(c(column_names$invalid_reason,
                                                              column_names$standard_concept,
                                                              column_names$concept_id,
                                                              column_names$concept_name,
                                                              "vocabulary",
                                                              column_names$domain_id,
                                                              column_names$concept_class_id)),
                                                     sep = " ",
                                                     remove = FALSE) %>%
                                        dplyr::select(!!into_id_colname := all_of(column_names$concept_id),
                                                      !!into,
                                                      !!!preserve_cols)


                                # If All NA concepts are not merged into a strip and returns a single NA
                                output <-
                                    output %>%
                                    dplyr::mutate_at(vars(!!into),
                                                     function(x) ifelse(grepl("NA NA \\[NA NA\\] \\[NA\\] \\[NA\\]",
                                                                              x,
                                                                              ignore.case = FALSE),
                                                                        NA_character_,
                                                                        x))


                                # Normalizing all NA to be able to get a flag for any mis-merged concepts
                                output <-
                                        output %>%
                                        tibble::as_tibble() %>%
                                        rubix::normalize_all_to_na()

                                # QA NA merges
                                qa <- output %>%
                                        dplyr::filter_at(vars(!!into_id_colname), all_vars(!is.na(.))) %>%
                                        dplyr::filter_at(vars(!!into), all_vars(is.na(.)))

                                if (nrow(qa)) {
                                        flagMergeStrip <<- qa
                                        warning(nrow(qa), ' where concept id is not <NA>, but merge strip is <NA>. See flagMergeStrip object.')
                                }




                                if (length(other_cols)) {

                                        output <-
                                                dplyr::bind_cols(output,
                                                                 .data %>%
                                                                     dplyr::select(all_of(other_cols)))

                                }


                                return(output)

            }

