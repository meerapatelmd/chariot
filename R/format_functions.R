#' Mutate all concept_ids to integer
#' @description Takes all the fields with "concept_id" in their names and converts to integer
#' @import dplyr
#' @export

ids_to_integer <-
    function(.data) {

        .data %>%
            dplyr::mutate_at(vars(contains("concept_id")),
                             as.integer)

    }


#' @title Filter Multiple Concept Strip Columns
#' @description
#' This function performs the same style of filtering as \code{\link{filterStrip}} over multiple columns.
#'
#' @param .data         Dataframe
#' @param merge_cols    Character vector of column names to filter
#' @param all           Equivalent to the `all_vars()` variable predicate in the Tidyverse system. If TRUE, all `merge_cols` are filtered for. If FALSE, the equivalent to `any_vars()` is performed. Default: TRUE
#' @param ...           Filter arguments passed to the dplyr filter function using the base Concept Table fields.
#' @return
#' A tibble.
#' @seealso
#' \code{\link{filterStrip}}
#' \code{\link[dplyr]{select}},\code{\link[dplyr]{select_all}},\code{\link[dplyr]{bind}},\code{\link[dplyr]{filter_all}},\code{\link[dplyr]{filter}},\code{\link[dplyr]{distinct}}
#'  \code{\link[tidyr]{separate_rows}}
#'  \code{\link[rubix]{normalize_all_to_na}}
#' @rdname filterAtStrip
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr select rename_at bind_cols filter_at filter distinct bind_rows
#' @importFrom tidyr separate_rows
#' @importFrom tibble rowid_to_column
#' @importFrom rubix normalize_all_to_na


filterAtStrip <-
        function(.data,
                 merge_cols,
                 all = TRUE,
                 ...) {


                data <-
                        .data %>%
                        tibble::rowid_to_column("filterAtStripId")


                reserveData <-
                        data

                inputData <-
                        data %>%
                        dplyr::select(filterAtStripId,
                                      all_of(merge_cols)) %>%
                        tidyr::pivot_longer(cols = !filterAtStripId,
                                            names_to = "merge_col",
                                            values_to = "Concept",
                                            values_drop_na = TRUE)

                inputData2 <-
                        inputData %>%
                        separateConceptStrip(Concept) %>%
                        tibble::rowid_to_column("filterAtStripId2")

                inputData3 <-
                        inputData2 %>%
                        unmergeStrip(strip_col = Concept)

                inputData4 <-
                        inputData3 %>%
                        dplyr::filter(...)


                if (all) {

                        inputData5 <-
                                inputData4 %>%
                                dplyr::select(filterAtStripId,
                                              merge_col,
                                              concept_id) %>%
                                dplyr::distinct() %>%
                                dplyr::filter(!is.na(concept_id)) %>%
                                tidyr::pivot_wider(
                                                   id_cols = filterAtStripId,
                                                   names_from = merge_col,
                                                   values_from = concept_id,
                                                   values_fn = list(concept_id = function(x) length(unique(x)))) %>%
                                dplyr::filter_at(vars(!filterAtStripId), all_vars(!is.na(.)))


                        return(reserveData[(reserveData$filterAtStripId %in% inputData5$filterAtStripId),] %>%
                                        dplyr::select(-contains("filterAtStripId")))

                } else {

                        return(
                        reserveData[(reserveData$filterAtStripId %in% inputData4$filterAtStripId),] %>%
                                dplyr::select(-contains("filterAtStripId")))


                }

                # column_names <-  c("concept_id",
                #                    "concept_name",
                #                    "domain_id",
                #                    "vocabulary_id",
                #                    "concept_class_id",
                #                    "standard_concept",
                #                    "concept_code",
                #                    "valid_start_date",
                #                    "valid_end_date",
                #                    "invalid_reason")

                # if (all) {
                #         for (i in 1:length(merge_cols)) {
                #
                #                 tcol <- merge_cols[i]
                #                 tmp_col <- paste0(tcol, "_tmp")
                #
                #                 tmp_data <-
                #                         .data %>%
                #                         dplyr::select(!!tcol) %>%
                #                         dplyr::rename_at(vars(1), ~paste(tmp_col))
                #
                #                 .data <-
                #                         .data %>%
                #                         dplyr::bind_cols(tmp_data) %>%
                #                         tidyr::separate_rows(!!tmp_col,
                #                                              sep = "\n") %>%
                #                         rubix::normalize_all_to_na() %>%
                #                         dplyr::filter_at(vars(!!tmp_col), all_vars(!is.na(.))) %>%
                #                         unmergeStrip(strip_col = !!tmp_col,
                #                                      remove = FALSE) %>%
                #                         dplyr::filter(...)  %>%
                #                         dplyr::select(-any_of(column_names)) %>%
                #                         dplyr::select(-!!tmp_col) %>%
                #                         dplyr::distinct()
                #
                #                 if (nrow(.data) == 0) {
                #                         return(.data)
                #                 }
                #
                #         }
                #
                #         return(.data)
                #
                # } else {
                #         .output <- list()
                #         for (i in 1:length(merge_cols)) {
                #
                #                 tcol <- merge_cols[i]
                #                 tmp_col <- paste0(tcol, "_tmp")
                #
                #                 tmp_data <-
                #                         .data %>%
                #                         dplyr::select(!!tcol) %>%
                #                         dplyr::rename_at(vars(1), ~paste(tmp_col))
                #
                #                 .output[[i]] <-
                #                         .data %>%
                #                         dplyr::bind_cols(tmp_data) %>%
                #                         tidyr::separate_rows(!!tmp_col,
                #                                              sep = "\n") %>%
                #                         rubix::normalize_all_to_na() %>%
                #                         dplyr::filter_at(vars(!!tmp_col), all_vars(!is.na(.))) %>%
                #                         unmergeStrip(strip_col = !!tmp_col,
                #                                      remove = FALSE) %>%
                #                         dplyr::filter(...)  %>%
                #                         dplyr::select(-any_of(column_names)) %>%
                #                         dplyr::select(-!!tmp_col) %>%
                #                         dplyr::distinct()
                #
                #         }
                #         .output <- dplyr::bind_rows(.output) %>%
                #                                 dplyr::distinct()
                #         return(.output)
                #
                #
                # }

        }




#' @title  Filter Columns with Merged Concept Strips
#' @description
#' This function filters a column that contains Concept Strips using Concept Table parameters. The target column may contain 1 or more merged concept strip, and the multiple strips must be separated by a new line \"\\n\" for the filter to operate correctly. It is important to note that the the filter is applied to the entire Concept Strip cell and will not alter the data content within the cell otherwise. For example, if the filter `vocabulary_id == 'RxNorm'` is used for `ColumnA`, a `ColumnA` cell that contains at least 1 RxNorm concept will be filtered for though there are other non-RxNorm concepts in that same cell.
#'
#' @param .data         dataframe with the merged concept column
#' @param merge_col     column of merged concepts
#' @param ...           arguments that will be passed to the dplyr filter function using the base Concept Table field names
#'
#' @return
#' A tibble with the same number of columns as the input with the number of rows equal or less than that of the input.
#'
#' @details
#' This function:
#' 1. Mutates a copy `merge_col` to a `merge_col_tmp` column,
#' 1. Separates the rows in `merge_col_tmp` by \"\\n\",
#' 1. Filters out any blanks and `NA` values introduced by the row separation,
#' 1. Unmerges the `merge_col` into the base Concept Table field names,
#' 1. Applies the filters found in the ellipses argument,
#' 1. Removes the `merge_col_tmp` and base Concept Table columns to reconstitute the original input, and
#' 1. Removes duplicate rows
#'
#' @seealso
#'  \code{\link[dplyr]{tidyeval-compat}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{filter_all}},\code{\link[dplyr]{filter}},\code{\link[dplyr]{select}},\code{\link[dplyr]{distinct}}
#'  \code{\link[rlang]{as_name}}
#'  \code{\link[tidyr]{separate_rows}}
#'  \code{\link[rubix]{normalize_all_to_na}}
#' @rdname filterStrip
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr enquo mutate filter_at filter select distinct
#' @importFrom rlang as_name
#' @importFrom tidyr separate_rows
#' @importFrom rubix normalize_all_to_na

filterStrip <-
    function(.data,
             merge_col,
             ...) {

            merge_col <- dplyr::enquo(merge_col)
            tmp_col <- paste0(rlang::as_name(merge_col), "tmp")


            column_names <-  c("concept_id",
                                      "concept_name",
                                      "domain_id",
                                      "vocabulary_id",
                                      "concept_class_id",
                                      "standard_concept",
                                      "concept_code",
                                      "valid_start_date",
                                      "valid_end_date",
                                      "invalid_reason")


            if (any(column_names %in% colnames(.data))) {

                    qa <- column_names[column_names %in% colnames(.data)]

                    stop('data cannot have any concept table column names: ', paste(qa, collapse = ", "))

            }

            .output <-
            .data %>%
                dplyr::mutate(!!tmp_col := !!merge_col) %>%
                separateConceptStrip(!!tmp_col) %>%
                # tidyr::separate_rows(!!tmp_col,
                #                      sep = "\n") %>%
                rubix::normalize_all_to_na() %>%
                dplyr::filter_at(vars(!!tmp_col), all_vars(!is.na(.))) %>%
                unmergeStrip(strip_col = !!tmp_col,
                             remove = FALSE) %>%
                dplyr::filter(...) %>%
                dplyr::select(-any_of(column_names)) %>%
                dplyr::select(-!!tmp_col) %>%
                dplyr::distinct()

            qa <- nrow(.output) > nrow(.data)

            if (qa) {
                    warning('returned data has more rows than input data')
            }

            return(.output)

    }




#' Get Merged Concept Id
#' @importFrom dplyr select
#' @export


getLabel <-
    function(concept_id,
             schema = "public") {
            queryConceptId(concept_ids = concept_id,
                           schema = schema) %>%
            makeLabel(into = "Label",
                      remove = TRUE) %>%
            dplyr::select(Label) %>%
            unlist() %>%
            unname()
    }




#' Concert a Label Column to a Merge Column
#' @import dplyr
#' @import tidyr
#' @import rubix
#' @export

labelToStrip <-
        function(.data,
                 label_col,
                 into,
                 remove = FALSE) {

                label_col <- enquo(label_col)
                into <- enquo(into)

                .data %>%
                        tidyr::extract(col = !!label_col,
                                        into = c("concept_id", "concept_name"),
                                        regex = "(^.*?) (.*$)",
                                        remove = remove) %>%
                        rubix::mutate_to_integer(concept_id) %>%
                        dplyr::rename(label_concept_id = concept_id) %>%
                        left_join_concept(column = "label_concept_id",
                                          include_synonyms = FALSE)  %>%
                        merge_concepts(into = !!into)

        }




#' Make Label Column
#' @description A Label is in the format of "{concept_id} concept_name". It is less comprehensive than a merged strip using the merge_concepts function, but more human readable when interfacing with others.
#' @importFrom tidyr unite
#' @import dplyr
#' @export

makeLabel <-
        function(.data,
                 into,
                 remove = FALSE) {

                into <- enquo(into)

                .data %>%
                        tidyr::unite(col = !!into,
                                     contains("concept_id"),
                                     contains("concept_name"),
                                     sep = " ",
                                     remove = remove) %>%
                        dplyr::mutate_at(vars(!!into), ~na_if(., "NA NA"))
        }




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
#' @importFrom tibble as_tibble
#' @export

mergeStrip <-
            function(.data,
                     into,
                     ...,
                     has_suffix = NULL,
                     has_prefix = NULL) {


                                into_id_colname <- paste0(into, "_id")

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
                                                stop("missing columns: ", paste(qa, collapse = ", "))
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
                                                      !!into)


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



                                if (!missing(...)) {
                                        output <-
                                                dplyr::bind_cols(output,
                                                                 .data %>%
                                                                         dplyr::select(!!!preserve_cols))



                                }




                                if (length(other_cols)) {

                                        output <-
                                                dplyr::bind_cols(output,
                                                                 .data %>%
                                                                     dplyr::select(all_of(other_cols)))

                                }


                                return(output)

            }





#' Parse a Concept Label
#' @description Parse a concept Label in the format of "{concept_id} {concept_name}".
#' @import tidyr
#' @import dplyr
#' @export





parseLabel <-
        function(.data,
                 label_col,
                 remove = FALSE) {


                label_col <- enquo(label_col)


                .data %>%
                        tidyr::extract(col = !!label_col,
                                       into = c("concept_id", "concept_name"),
                                       regex = "(^.*?) (.*$)",
                                       remove = remove)

        }




#' @title Separate Concept Strips by Row
#' @description
#' This function separates a merged Concept Strip Column into rows by new line \"\\n\".
#' @param .data A data frame.
#' @param ... Columns to separate across multiple rows that are passed to \code{\link[tidyr]{separate_rows}}.
#' @return
#' A longer tibble if the merged Concept Strip Column/s have greater than 1 Concept Strips.
#' @seealso
#'  \code{\link[tidyr]{separate_rows}}
#' @rdname separateConceptStrip
#' @export
#' @importFrom tidyr separate_rows

separateConceptStrip <-
        function(.data,
                 ...) {

                tidyr::separate_rows(.data,
                                     ...,
                                     sep = "(?<=\\])\n(?=\\[A-Z\\])")
        }




#' Convert a Merge Strip to a Label
#' @import dplyr
#' @export

stripToLabel <-
        function(.data,
                 merge_col,
                 into,
                 remove = FALSE) {

                merge_col <- enquo(merge_col)
                into <- enquo(into)

                unmerge_concepts(dataframe = .data,
                                          concept_col = !!merge_col,
                                          remove = remove) %>%
                        makeLabel(into = !!into,
                                  remove = remove)
        }




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


#' Get Merged Concept Id
#' @importFrom dplyr select
#' @export


getMerge <-
    function(concept_id,
             schema = "public") {

        queryConceptId(concept_ids = concept_id,
                       schema = schema) %>%
                mergeStrip(into = "Concept") %>%
                dplyr::select("Concept") %>%
                unlist() %>%
                unname()

    }

