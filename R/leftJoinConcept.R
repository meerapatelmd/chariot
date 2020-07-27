#' Join a dataframe object with Athena's concept table
#' @description This function has better performance than the WHERE IN statement for larger searches. A table in the format of "v{unpunctuated timestamp}" is written to the local Athena. A join is performed with a concept table. The table is subsequently dropped. If a dataframe column is not provided as a the column to join the concept table on, the 1st column will be used by default.
#' @param dataframe dataframe to join
#' @param dataframe_column string of the column name to join on. If NULL, the 1st column is use
#' @param concept_column name of concept_column to join dataframe on. Defaults to concept ID.
#' @importFrom seagull create_table_via_temp_file
#' @importFrom seagull drop_table
#' @export


leftJoinConcept <-
    function(.data,
             column = NULL,
             athena_schema = "public",
             concept_column = "concept_id",
             synonyms = FALSE,
             override_cache = FALSE) {


                    if (is.null(column)) {
                        column <- colnames(.data)[1]
                    }

                    if (column == concept_column) {
                        stop("'column' parameter cannot be equal to 'concept_column'")
                    }

                    .output <-
                        leftJoin(.data = .data,
                                 column = column,
                                 athena_schema = athena_schema,
                                 athena_table = "concept",
                                 athena_column = concept_column,
                                 override_cache = override_cache)


                    if (synonyms) {

                        .output_synonyms <-
                                leftJoin(.output %>%
                                             dplyr::select(all_of(column)),
                                         column = column,
                                         athena_schema = athena_schema,
                                         athena_table = "concept_synonym",
                                         athena_column = "concept_id",
                                         override_cache = override_cache) %>%
                                dplyr::select(-language_concept_id) %>%
                                dplyr::distinct()

                        .output2 <-
                            dplyr::left_join(.output,
                                             .output_synonyms) %>%
                            dplyr::filter(concept_name != concept_synonym_name) %>%
                            dplyr::distinct() %>%
                            rubix::group_by_unique_aggregate(concept_id,
                                                             agg.col = concept_synonym_name)

                        .output <-
                                dplyr::left_join(.output,
                                                 .output2)

                    }

                    return(.output)

    }
