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
