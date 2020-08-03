#' Merge OMOP Concepts into a Strip
#' @description This function takes a set of the OMOP Vocabulary Concept Table fields and merges all of them except for the date fields into a single Concept "Strip". If the Strip output is `<NA>` while the input concept id is not, a flagMergeStrip object is returned in the Global Environment.
#' @return A tibble with all blank and "NA" normalized to `<NA>` with 1. If present, `valid_start_date` and `valid_end_date` fields are permanently removed, 2. 8 out of the 10 remaining Concept Table fields (concept_id, concept_name, domain_id, vocabulary_id, concept_class_id, standard_concept, concept_code, invalid_reason) are merged into a single column with the provided column name, 3. the concept_id column is renamed to the format of the provided merged column name: {into_}concept_id. The remaining of the 7 Concept Table fields may also be preserved outside of the merge if provided. All other columns present in the input data are returned along with the transformations described.
#' @param .data dataframe with the following required fields from the output
#' @param into name of the column that the new combined string will be in
#' @param ... columns other than concept_id that will be removed in tidyr unite but should be preserved in addition to be merged.
#' @param has_suffix if the omop concept element column names are different from the standard by a suffix, include it so it can point to the correct set of columns
#' @param has_prefix if the omop concept element column names are prefixed, include it so it can point to the correct set of columns
#' @param remove If TRUE, remove any possible Concept Table fields in the output, leaving only the newly created label field. All other fields will stay in the output.
#' @import dplyr
#' @import tidyr
#' @importFrom tibble as_tibble
#' @export

mergeLabel <-
            function(.data,
                     into,
                     has_suffix = NULL,
                     has_prefix = NULL,
                     remove = TRUE) {


                                # Enquo output column name
                                into <- dplyr::enquo(into)


                                # Generating a list of concept table columns that includes prefixes and suffixes
                                column_names <- paste0(has_prefix,
                                                        c("concept_id",
                                                         "concept_name"
                                                         # ,
                                                         # "domain_id",
                                                         # "vocabulary_id",
                                                         # "concept_class_id",
                                                         # "standard_concept",
                                                         # "concept_code",
                                                         # "valid_start_date",
                                                         # "valid_end_date",
                                                         # "invalid_reason"
                                                         )
                                                       ,
                                                       has_suffix) %>%
                                                as.list()

                                names(column_names) <-  c("concept_id",
                                                          "concept_name"
                                                          # ,
                                                          # "domain_id",
                                                          # "vocabulary_id",
                                                          # "concept_class_id",
                                                          # "standard_concept",
                                                          # "concept_code",
                                                          # "valid_start_date",
                                                          # "valid_end_date",
                                                          # "invalid_reason"
                                                          )

                                if (!(all(unlist(column_names) %in% colnames(.data)))) {

                                        qa <- unlist(column_names)[!(unlist(column_names) %in% colnames(.data))]

                                        if (length(qa)) {
                                                stop("missing columns: ", qa)
                                        }

                                }

                                output <-
                                .data %>%
                                        tidyr::unite(col = !!into,
                                                     all_of(c(column_names$concept_id,
                                                              column_names$concept_name)),
                                                     sep = " ",
                                                     remove = FALSE)


                                # If All NA concepts are not merged into a strip and returns a single NA
                                output <-
                                    output %>%
                                    dplyr::mutate_at(vars(!!into),
                                                     function(x) ifelse(grepl("NA NA",
                                                                              x,
                                                                              ignore.case = FALSE),
                                                                        NA_character_,
                                                                        x))


                                # Normalizing all NA to be able to get a flag for any mis-merged concepts
                                output <-
                                        output %>%
                                        tibble::as_tibble() %>%
                                        dplyr::mutate_all(as.character) %>%
                                        rubix::normalize_all_to_na()

                                # QA NA merges
                                qa <- output %>%
                                        dplyr::filter_at(vars(all_of(column_names$concept_id)), all_vars(!is.na(.))) %>%
                                        dplyr::filter_at(vars(!!into), all_vars(is.na(.)))

                                if (nrow(qa)) {
                                        flagMergeLabel <<- qa
                                        warning(nrow(qa), ' where concept id is not <NA>, but label is <NA>. See flagMergeLabel object.')
                                }

                                if (remove) {

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
                                                                 "invalid_reason"
                                                               )
                                                               ,
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
                                                                  "invalid_reason"
                                        )


                                        output <-
                                                output %>%
                                                dplyr::select_at(vars(!any_of(c(column_names$concept_id,
                                                                     column_names$concept_name,
                                                                     column_names$domain_id,
                                                                     column_names$vocabulary_id,
                                                                     column_names$concept_class_id,
                                                                     column_names$standard_concept,
                                                                     column_names$concept_code,
                                                                     column_names$valid_start_date,
                                                                     column_names$valid_end_date,
                                                                     column_names$invalid_reason))))


                                }



                                return(output)

            }

