#' @title =
#' Get Concept Strip
#' @example inst/example/format_merge.R
#' @seealso
#'  \code{\link[dplyr]{select}}
#' @rdname get_strip
#' @export
#' @importFrom dplyr select

get_strip <-
    function(concept_id,
             vocab_schema = "omop_vocabulary",
             conn) {

        lookup_concept_id(concept_id = concept_id,
                       vocab_schema = vocab_schema,
                       conn = conn) %>%
            merge_strip(into = "Concept") %>%
            dplyr::select("Concept") %>%
            unlist() %>%
            unname()

    }

#' @title
#' Unbox Strip
#' @description
#' First separates rows by the `row_sep` argument, followed by unmerging the strip
#' @seealso
#'  \code{\link[tidyr]{separate_rows}}
#' @rdname unbox_strip
#' @export
#' @importFrom tidyr separate_rows
#' @example inst/example/format_merge.R

unbox_strip <-
    function(data,
             strip_col,
             sep = "\n",
             add_suffix = NULL,
             add_prefix = NULL,
             remove = TRUE,
             r_trimws = TRUE) {


            # test_data <-
            #     tibble::tibble(Concept = "[V] [S] 1112807 aspirin [RxNorm 1191] [Drug] [Ingredient]\n[V] [S] 1112807 aspirin [RxNorm 1191] [Drug] [Ingredient]")

            data %>%
                 tidyr::separate_rows({{ strip_col }}, sep = sep) %>%
                    unmerge_strip(strip_col = {{ strip_col }},
                                  add_suffix = add_suffix,
                                  add_prefix = add_prefix,
                                  remove = remove,
                                  r_trimws = r_trimws)



    }


#' @title
#' Unbox Strip
#' @description
#' First separates rows by the `row_sep` argument, followed by unmerging the strip
#' @seealso
#'  \code{\link[tidyr]{separate_rows}}
#' @rdname unbox_label
#' @export
#' @importFrom tidyr separate_rows extract


unbox_label <-
    function(data,
             label_col,
             row_sep = "\n",
             remove = FALSE) {


        # test_data <-
        #     tibble::tibble(Concept = "1112807 aspirin\n1112807 aspirin")

        data %>%
            tidyr::separate_rows({{ label_col }}, sep = row_sep) %>%
            tidyr::extract(col = {{ label_col }},
                           into = c("concept_id", "concept_name"),
                           regex = "(^.*?) (.*$)",
                           remove = remove)



    }


#' @title
#' Filter Multiple Concept Strip Columns
#' @description
#' This function performs the same style of filtering as \code{\link{filter_strip}} over multiple columns.
#'
#' @param data         Dataframe
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
#' @rdname filter_at_all_strip
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr select rename_at bind_cols filter_at filter distinct bind_rows
#' @importFrom tidyr separate_rows
#' @importFrom tibble rowid_to_column
#' @importFrom rubix normalize_all_to_na
#' @example inst/example/format_filter_strip.R

filter_at_all_strip <-
        function(data,
                 strip_cols,
                 ...) {


            for (i in seq_along(strip_cols)) {

                strip_col <- strip_cols[i]

                data <-
                filter_strip(data,
                             strip_col = {{ strip_col }},
                             ...)


            }

            data

        }


#' @rdname filter_at_any_strip
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr select rename_at bind_cols filter_at filter distinct bind_rows
#' @importFrom tidyr separate_rows
#' @importFrom tibble rowid_to_column
#' @importFrom rubix normalize_all_to_na
#' @example inst/example/format_filter_strip.R

filter_at_any_strip <-
    function(data,
             strip_cols,
             ...) {


        output <- list()
        for (i in seq_along(strip_cols)) {

            strip_col <- strip_cols[i]

            output[[i]] <-
                filter_strip(data,
                             strip_col = {{ strip_col }},
                             ...)


        }

        dplyr::bind_rows(output) %>%
            dplyr::distinct()

    }

#' @title  Filter Columns with Merged Concept Strips
#' @description
#' This function filters a column that contains Concept Strips using Concept Table parameters. The target column may contain 1 or more merged concept strip, and the multiple strips must be separated by a new line \"\\n\" for the filter to operate correctly. It is important to note that the the filter is applied to the entire Concept Strip cell and will not alter the data content within the cell otherwise. For example, if the filter `vocabulary_id == 'RxNorm'` is used for `ColumnA`, a `ColumnA` cell that contains at least 1 RxNorm concept will be filtered for though there are other non-RxNorm concepts in that same cell.
#'
#' @param data         dataframe with the merged concept column
#' @param merge_col     column of merged concepts
#' @param ...           arguments that will be passed to the dplyr filter function using the base Concept Table field names
#'
#' @return
#' A tibble with the same number of columns as the input with the number of rows equal or less than that of the input.
#'
#' @seealso
#'  \code{\link[dplyr]{tidyeval-compat}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{filter_all}},\code{\link[dplyr]{filter}},\code{\link[dplyr]{select}},\code{\link[dplyr]{distinct}}
#'  \code{\link[rlang]{as_name}}
#'  \code{\link[tidyr]{separate_rows}}
#'  \code{\link[rubix]{normalize_all_to_na}}
#' @rdname filter_strip
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr enquo mutate filter_at filter select distinct
#' @importFrom rlang as_name
#' @importFrom tidyr separate_rows
#' @importFrom rubix normalize_all_to_na
#' @example inst/example/format_filter_strip.R

filter_strip <-
    function(data,
             strip_col,
             ...) {


            column_names <-  c("concept_id",
                                      "concept_name",
                                      "domain_id",
                                      "vocabulary_id",
                                      "concept_class_id",
                                      "standard_concept",
                                      "concept_code",
                                      "invalid_reason")

            if (any(column_names %in% colnames(data))) {

                    stop("redundant column names")
            }

                data %>%
                unmerge_strip(strip_col = {{ strip_col }},
                              remove = FALSE) %>%
                dplyr::filter(...) %>%
                dplyr::select(!dplyr::any_of(column_names))
            # column_names <-  c("concept_id",
            #                           "concept_name",
            #                           "domain_id",
            #                           "vocabulary_id",
            #                           "concept_class_id",
            #                           "standard_concept",
            #                           "concept_code",
            #                           "valid_start_date",
            #                           "valid_end_date",
            #                           "invalid_reason")
            #
            #
            # if (any(column_names %in% colnames(data))) {
            #
            #         qa <- column_names[column_names %in% colnames(data)]
            #
            #         stop('data cannot have any concept table column names: ', paste(qa, collapse = ", "))
            #
            # }
            #
            # .output <-
            # data %>%
            #     dplyr::mutate(!!tmp_col := !!merge_col) %>%
            #     separateConceptStrip(!!tmp_col) %>%
            #     # tidyr::separate_rows(!!tmp_col,
            #     #                      sep = "\n") %>%
            #     rubix::normalize_all_to_na() %>%
            #     dplyr::filter_at(dplyr::vars(!!tmp_col), dplyr::all_vars(!is.na(.))) %>%
            #     unmergeStrip(strip_col = !!tmp_col,
            #                  remove = FALSE) %>%
            #     dplyr::filter(...) %>%
            #     dplyr::select(-any_of(column_names)) %>%
            #     dplyr::select(-!!tmp_col) %>%
            #     dplyr::distinct()
            #
            # qa <- nrow(.output) > nrow(data)
            #
            # if (qa) {
            #         warning('returned data has more rows than input data')
            # }
            #
            # return(.output)

    }



#' Concert a Label Column to a Merge Column
#' @seealso
#'  \code{\link[tidyr]{extract}}
#' @rdname label_to_strip
#' @export
#' @importFrom tidyr extract

label_to_strip <-
        function(data,
                 label_col,
                 into_strip_col,
                 remove = FALSE) {

                # Other than the concept_id and concept_name that will be derived from the label column, are the other required columns present?
                concept_fields <-
                c(#"concept_id",
                  #"concept_name",
                  "domain_id",
                  "vocabulary_id",
                  "concept_class_id",
                  "standard_concept",
                  "concept_code",
                  "invalid_reason")

                if (!(all(concept_fields %in% colnames(data)))) {

                    stop("missing required fields")

                }

                if (any(c("concept_id", "concept_name") %in% colnames(data))) {

                    stop("cannot unmerge label_col with `concept_id` and `concept_name` already in the data")
                }

                data %>%
                        tidyr::extract(col = {{ label_col }},
                                        into = c("concept_id", "concept_name"),
                                        regex = "(^.*?) (.*$)",
                                        remove = remove) %>%
                        merge_strip(into = {{ into_strip_col }})

        }




#' Make Label Column
#' @description A Label is in the format of "{concept_id} concept_name". It is less comprehensive than a merged strip using the merge_concepts function, but more human readable when interfacing with others.
#' @seealso
#'  \code{\link[tidyr]{unite}}
#'  \code{\link[dplyr]{reexports}},\code{\link[dplyr]{mutate_all}},\code{\link[dplyr]{vars}}
#' @rdname merge_label
#' @export
#' @importFrom tidyr unite
#' @importFrom dplyr all_of mutate_at vars

merge_label <-
        function(data,
                 into,
                 prefix = NULL,
                 suffix = NULL,
                 remove = TRUE) {


                label_parts <- paste0(prefix, c("concept_id", "concept_name"), suffix)
                names(label_parts) <- c("concept_id", "concept_name")


                data %>%
                        tidyr::unite(col = {{into}},
                                     dplyr::all_of(label_parts$concept_id),
                                     dplyr::all_of(label_parts$concept_name),
                                     sep = " ",
                                     na.rm = TRUE,
                                     remove = remove) %>%
                        dplyr::mutate_at(dplyr::vars({{into}}), ~na_if(., "NA NA"))
        }


#' Merge OMOP Concepts into a Strip
#' @description
#' All the OMOP Vocabulary Concept Table fields other than the date fields are "merged"into a single string, called a "Strip". If the Strip output is `<NA>` while the input concept id is not, a flagMergeStrip object is returned in the Global Environment.
#' @return A tibble with all blank and "NA" normalized to `<NA>` with 1. If present, `valid_start_date` and `valid_end_date` fields are permanently removed, 2. 8 out of the 10 remaining Concept Table fields (concept_id, concept_name, domain_id, vocabulary_id, concept_class_id, standard_concept, concept_code, invalid_reason) are merged into a single column with the provided column name, 3. the concept_id column is renamed to the format of the provided merged column name: {into_}concept_id. The remaining of the 7 Concept Table fields may also be preserved outside of the merge if provided. All other columns present in the input data are returned along with the transformations described.
#' @param data dataframe with the following required fields from the output
#' @param into name of the column that the new combined string will be in
#' @param ... columns other than concept_id that will be removed in tidyr unite but should be preserved in addition to be merged.
#' @param suffix if the omop concept element column names are different from the standard by a suffix, include it so it can point to the correct set of columns
#' @param prefix if the omop concept element column names are prefixed, include it so it can point to the correct set of columns
#' @import dplyr
#' @import tidyr
#' @importFrom tibble as_tibble
#' @export
#' @rdname merge_strip

merge_strip <-
            function(data,
                     into,
                     ...,
                     suffix = NULL,
                     prefix = NULL) {


                                into_id_colname <- paste0(into, "_id")

                                # Enquo output column name
                                into <- dplyr::enquo(into)
                                # Preserve columns
                                preserve_cols <- dplyr::enquos(...)


                                # Generating a list of concept table columns that includes prefixes and suffixes
                                column_names <- paste0(prefix,
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
                                                       suffix) %>%
                                                as.list()

                                concept_fields <-  c("concept_id",
                                                          "concept_name",
                                                          "domain_id",
                                                          "vocabulary_id",
                                                          "concept_class_id",
                                                          "standard_concept",
                                                          "concept_code",
                                                          "valid_start_date",
                                                          "valid_end_date",
                                                          "invalid_reason")

                                names(column_names) <- concept_fields


                                if (!(all(unlist(column_names) %in% colnames(data)))) {

                                        stop(sprintf("missing column names: %s", paste(unlist(column_names), collapse = ", ")))

                                }

                                # All other column names
                                other_cols <<- colnames(data)[!(colnames(data) %in% unlist(column_names))]


                                output <-
                                data %>%
                                        dplyr::mutate_at(dplyr::vars(dplyr::all_of(column_names$standard_concept)), function(x) ifelse(is.na(x), "N", x)) %>%
                                        dplyr::mutate_at(dplyr::vars(dplyr::all_of(column_names$standard_concept)), function(x) paste0("[", x, "]")) %>%
                                        dplyr::mutate_at(dplyr::vars(dplyr::all_of(column_names$invalid_reason)), function(x) ifelse(is.na(x), "[V]", paste0("[", x, "]"))) %>%
                                        tidyr::unite(col = vocabulary,
                                                     dplyr::all_of(c(column_names$vocabulary_id,
                                                              column_names$concept_code)),
                                                     sep = " ") %>%
                                        dplyr::mutate_at(dplyr::vars(dplyr::all_of(c(column_names$domain_id,
                                                                       "vocabulary",
                                                                       column_names$concept_class_id))),
                                                         function(x) paste0("[", x, "]")) %>%
                                        #dplyr::select_at(dplyr::vars(!matches("valid.*date"))) %>%
                                        tidyr::unite(col = {{ into }},
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
                                                      {{ into }})


                                # If All NA concepts are not merged into a strip and returns a single NA
                                output <-
                                    output %>%
                                    dplyr::mutate_at(dplyr::vars({{ into }}),
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
                                        dplyr::filter_at(dplyr::vars(!!into_id_colname), dplyr::all_vars(!is.na(.))) %>%
                                        dplyr::filter_at(dplyr::vars({{ into }}), dplyr::all_vars(is.na(.)))

                                if (nrow(qa)) {
                                        flagMergeStrip <<- qa
                                        warning(nrow(qa), ' where concept id is not <NA>, but merge strip is <NA>. See flagMergeStrip object.')
                                }



                                if (!missing(...)) {
                                        output <-
                                                dplyr::bind_cols(output,
                                                                 data %>%
                                                                         dplyr::select(!!!preserve_cols))



                                }




                                if (length(other_cols)) {

                                        output <-
                                                dplyr::bind_cols(output,
                                                                 data %>%
                                                                     dplyr::select(dplyr::all_of(other_cols)))

                                }


                                return(output)

            }





#' Parse a Concept Label
#' @description
#' Parse a concept Label in the format of "{concept_id} {concept_name}".
#' @seealso
#'  \code{\link[tidyr]{extract}}
#' @rdname unmerge_label
#' @export
#' @importFrom tidyr extract


unmerge_label <-
        function(data,
                 label_col,
                 remove = FALSE) {

                data %>%
                        tidyr::extract(col = {{ label_col }},
                                       into = c("concept_id", "concept_name"),
                                       regex = "(^.*?) (.*$)",
                                       remove = remove)

        }

#' Convert a Merge Strip to a Label
#' @rdname strip_to_label
#' @export

strip_to_label <-
        function(data,
                 strip_col,
                 into_label_col,
                 remove = FALSE) {

                unmerge_strip(data,
                              strip_col = {{ strip_col }},
                              remove = remove) %>%
                        merge_label(into = {{ into_label_col }},
                                  remove = remove)
        }




#' Unmerge OMOP Concept Strip
#' @description This function unmerges an OMOP concept strip created by a 'merge' function using the tidyr extract function. If the input is not a tibble, it will be converted into one with the blanks and "NA" values normalized to `<NA>`. A warning is returned in the console if some concepts fail to unmerge into their respective concept table fields, as determined by all the new column fields having a value of `<NA>` with a non-`<NA>` value in the strip_col instance inputed. Errors will be thrown if the data input already contains columns that will collide with the new columns, the names of which defaults to the names of the original concept table fields: concept_id, concept_name, domain_id, vocabulary_id, concept_class_id, standard_concept, concept_code, invalid_reason. Note that the original concept table fields `valid_start_date` and `valid_end_date` are the only concept table fields are not a requirement in the merge and unmerging process.
#' @return a tibble with all blanks, "NA", and <NA> normalized to NA, with unmerged columns with or without the provided prefix and suffix pasted in postions 1 to 8, followed by the strip column if the remove parameter is FALSE, and the remaining fields present in the input.
#' @param data dataframe
#' @param strip_col column that contains the merged concept strip
#' @param remove remove argument passed to the tidyr extract function. If TRUE, removes strip_col in output.
#' @param r_trimws Due to some of the carriage returns in aggregate transformations and other edits in Excel, r_trimws is an argument that if TRUE, trims right whitespace of the freshly unmerged columns for any trailing carriage returns.
#' @importFrom tidyr extract
#' @import dplyr
#' @importFrom stringr str_remove_all
#' @importFrom tibble as_tibble
#' @importFrom rubix normalize_all_to_na
#' @export
#' @rdname unmerge_strip

unmerge_strip <-
    function(data,
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

                    if (any(unlist(new_cols) %in% colnames(data))) {
                            qa <- unlist(new_cols)[unlist(new_cols) %in% colnames(data)]
                            stop('new column names already present: ', paste(qa, collapse = ", "))
                    }

                    output <-
                    data %>%
                        tidyr::extract(col = !!strip_col,
                                       remove = FALSE,
                                       into = unlist(new_cols),
                                       regex = "(\\[.{1}\\]) (\\[.{1}\\]) ([^ ]*) (.*?) (\\[.*?) (.*?\\]) (\\[.*?\\]) (\\[.*?\\])") %>%
                           tibble::as_tibble() %>%
                            rubix::normalize_all_to_na()

                    output <-
                        output %>%
                                dplyr::mutate_at(dplyr::vars(dplyr::all_of(unlist(new_cols))), stringr::str_remove_all, "^\\[|\\]$") %>%
                                dplyr::mutate_at(dplyr::vars(new_cols$standard_concept, new_cols$invalid_reason), stringr::str_replace_all, "^N$|^V$", NA_character_) %>%
                                dplyr::select(dplyr::all_of(c(new_cols$concept_id,
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
                                dplyr::mutate_at(dplyr::vars(dplyr::all_of(c(new_cols$concept_id,
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
                        dplyr::filter_at(dplyr::vars(c(new_cols$concept_id,
                                                new_cols$concept_name,
                                                new_cols$domain_id,
                                                new_cols$vocabulary_id,
                                                new_cols$concept_class_id,
                                                new_cols$standard_concept,
                                                new_cols$concept_code,
                                                new_cols$invalid_reason)),
                                         dplyr::all_vars(is.na(.))) %>%
                        dplyr::filter_at(dplyr::vars(!!strip_col),
                                         dplyr::all_vars(!is.na(.)))

                    if (nrow(qa) > 0) {


                            flagUnmergeStrip <<- qa

                            warning('Not all concepts unmerged: ', nrow(qa), '. See flagUnmergeStrip object.')


                    }

                    if (remove) {

                        output <-
                            output %>%
                            dplyr::select(-!!strip_col)

                    }

                    output

    }


