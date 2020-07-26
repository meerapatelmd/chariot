#
# test_data <-
#         chariot::query_athena("SELECT * FROM cancergov.concept") %>%
#         dplyr::select(test_concept_id = concept_id)
#
# chariot::leftJoin(test_data,
#                   athena_schema = "public",
#                   athena_table = "concept",
#                   athena_column = "concept_id",
#                   where_athena_col = "vocabulary_id",
#                   where_athena_col_in = c("RxNorm", "HemOnc"))
