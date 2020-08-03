#' Unmerge OMOP Concept Strip
#' @description This function unmerges an OMOP concept strip created by a 'merge' function using the tidyr extract function. If the input is not a tibble, it will be converted into one with the blanks and "NA" values normalized to `<NA>`. A warning is returned in the console if some concepts fail to unmerge into their respective concept table fields, as determined by all the new column fields having a value of `<NA>` with a non-`<NA>` value in the strip_col instance inputed. Errors will be thrown if the .data input already contains columns that will collide with the new columns, the names of which defaults to the names of the original concept table fields: concept_id, concept_name, domain_id, vocabulary_id, concept_class_id, standard_concept, concept_code, invalid_reason. Note that the original concept table fields `valid_start_date` and `valid_end_date` are the only concept table fields are not a requirement in the merge and unmerging process.
#' @return a tibble with all blanks, "NA", and <NA> normalized to NA, with unmerged columns with or without the provided prefix and suffix pasted in postions 1 to 8, followed by the strip column if the remove parameter is FALSE, and the remaining fields present in the input.
#' @param .data dataframe
#' @param strip_col column that contains the merged concept strip
#' @param remove remove argument passed to the tidyr extract function. If TRUE, removes strip_col in output.
#' @param r_trimws Due to some of the carriage returns in aggregate transformations and other edits in Excel, r_trimws is an argument that if TRUE, trims right whitespace of the freshly unmerged columns for any trailing carriage returns.
#' @importFrom tidyr extract
#' @import dplyr
#' @importFrom stringr str_remove_all
#' @importFrom tibble as_tibble
#' @importFrom rubix normalize_all_to_na
#' @export

unmergeStrip <-
    function(.data,
             strip_col,
             add_suffix = NULL,
             add_prefix = NULL,
             remove = TRUE,
             r_trimws = TRUE) {

                    strip_col <- dplyr::enquo(strip_col)

                    colOrder <- c("invalid_reason",
                                  "standard_concept",
                                  "concept_id",
                                  "concept_name",
                                  "vocabulary_id",
                                  "concept_code",
                                  "domain_id",
                                  "concept_class_id")

                    new_cols <- paste0(add_prefix,
                                       colOrder,
                                       add_suffix) %>%
                                as.list()

                    names(new_cols) <- colOrder

                    new_cols <- new_cols

                    if (any(unlist(new_cols) %in% colnames(.data))) {
                            qa <- unlist(new_cols)[unlist(new_cols) %in% colnames(.data)]
                            stop('new column names already present: ', paste(qa, collapse = ", "))
                    }

                    output <-
                    .data %>%
                        tidyr::extract(col = !!strip_col,
                                       remove = FALSE,
                                       into = unlist(new_cols),
                                       regex = "(\\[.{1}\\]) (\\[.{1}\\]) ([^ ]*) (.*?) (\\[.*?) (.*?\\]) (\\[.*?\\]) (\\[.*?\\])") %>%
                           tibble::as_tibble() %>%
                            rubix::normalize_all_to_na()

                    output <-
                        output %>%
                                dplyr::mutate_at(vars(all_of(unlist(new_cols))), stringr::str_remove_all, "^\\[|\\]$") %>%
                                dplyr::mutate_at(vars(new_cols$standard_concept, new_cols$invalid_reason), stringr::str_replace_all, "^N$|^V$", NA_character_) %>%
                                dplyr::select(all_of(c(new_cols$concept_id,
                                                       new_cols$concept_name,
                                                       new_cols$domain_id,
                                                       new_cols$vocabulary_id,
                                                       new_cols$concept_class_id,
                                                       new_cols$standard_concept,
                                                       new_cols$concept_code,
                                                       new_cols$invalid_reason)),
                                              dplyr::everything())

                    if (r_trimws == TRUE) {

                            output <-
                                output %>%
                                dplyr::mutate_at(vars(all_of(c(new_cols$concept_id,
                                                               new_cols$concept_name,
                                                               new_cols$domain_id,
                                                               new_cols$vocabulary_id,
                                                               new_cols$concept_class_id,
                                                               new_cols$standard_concept,
                                                               new_cols$concept_code,
                                                               new_cols$invalid_reason))),
                                                 base::trimws)

                    }

                    qa <-
                        output %>%
                        dplyr::filter_at(vars(c(new_cols$concept_id,
                                                new_cols$concept_name,
                                                new_cols$domain_id,
                                                new_cols$vocabulary_id,
                                                new_cols$concept_class_id,
                                                new_cols$standard_concept,
                                                new_cols$concept_code,
                                                new_cols$invalid_reason)),
                                         all_vars(is.na(.))) %>%
                        dplyr::filter_at(vars(!!strip_col),
                                         all_vars(!is.na(.)))

                    if (nrow(qa) > 0) {


                            flagUnmergeStrip <<- qa

                            warning('Not all concepts unmerged: ', nrow(qa), '. See flagUnmergeStrip object.')


                    }

                    if (remove) {

                        output <-
                            output %>%
                            dplyr::select(-!!strip_col)

                    }

                    return(output)

    }
