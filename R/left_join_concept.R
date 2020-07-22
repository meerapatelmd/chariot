#' Join a dataframe object with Athena's concept table
#' @description This function has better performance than the WHERE IN statement for larger searches. A table in the format of "v{unpunctuated timestamp}" is written to the local Athena. A join is performed with a concept table. The table is subsequently dropped. If a dataframe column is not provided as a the column to join the concept table on, the 1st column will be used by default.
#' @param dataframe dataframe to join
#' @param dataframe_column string of the column name to join on. If NULL, the 1st column is use
#' @param concept_column name of concept_column to join dataframe on. Defaults to concept ID.
#' @importFrom seagull create_table_via_temp_file
#' @importFrom seagull drop_table
#' @export


left_join_concept <-
    function(.data,
             column = NULL,
             concept_column = "concept_id",
             include_synonyms = TRUE,
             omop = FALSE,
             omop_schema = "omop_vocabulary",
             override_cache = FALSE) {



        if (omop) {
            output <-
                left_join_df_omop(.data = .data,
                             column = column,
                             athena_table = "concept",
                             athena_column = concept_column,
                             omop_schema = omop_schema,
                             override_cache = override_cache)


            if (include_synonyms) {

                output_b <-
                    left_join_df_omop(.data,
                                 column = column,
                                 athena_table = "concept_synonym",
                                 athena_column = "concept_id",
                                 omop_schema = omop_schema,
                                 override_cache = override_cache) %>%
                    dplyr::filter(language_concept_id == "4180186") %>%
                    # deduping
                    dplyr::select(concept_id,concept_synonym_name) %>%
                    dplyr::distinct()

                # Combine concept and concept_synonym resultsets and filter out values where the synonym and concept_name are the same
                output_b2 <-
                    dplyr::left_join(output,
                                     output_b,
                                     by = "concept_id") %>%
                    dplyr::filter(concept_name != concept_synonym_name) %>%
                    rubix::group_by_unique_aggregate(concept_id,
                                                     agg.col = concept_synonym_name)


                output <-
                    output %>%
                    dplyr::left_join(output_b2,
                                     by = "concept_id")
            }






        } else {

                output <-
                left_join_df(.data = .data,
                             column = column,
                             athena_table = "concept",
                             athena_column = concept_column)


                if (include_synonyms) {

                        output_b <-
                                left_join_df(.data,
                                             column = column,
                                             athena_table = "concept_synonym",
                                             athena_column = "concept_id",
                                             override_cache = override_cache) %>%
                                dplyr::filter(language_concept_id == "4180186") %>%
                                # deduping
                                dplyr::select(concept_id,concept_synonym_name) %>%
                                dplyr::distinct()

                    # Combine concept and concept_synonym resultsets and filter out values where the synonym and concept_name are the same
                                output_b2 <-
                                        dplyr::left_join(output,
                                                         output_b,
                                                         by = "concept_id") %>%
                                        dplyr::filter(concept_name != concept_synonym_name) %>%
                                        rubix::group_by_unique_aggregate(concept_id,
                                                                         agg.col = concept_synonym_name)


                                output <-
                                    output %>%
                                    dplyr::left_join(output_b2,
                                                     by = "concept_id")
                }
        }


        return(output)
    }
