#' Pivot all relatives of a set of concepts
#' @description This function takes a dataframe and mutates an additional column providing the specimen type based on the "Has system" relationship id.
#' @param concept_id_column The column in dataframe that points to the concept_id. If NULL, defaults to "concept_id".
#' @param dataframe input data
#' @param names_from concept table column to be pivoted on
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr left_join
#' @importFrom dplyr distinct
#' @export

pivotRelative <-
    function(.data,
             athena_schema = "public",
             column = NULL,
             whereLevelIn = NULL,
             whereLevelType = NULL,
             names_from,
             include_count = TRUE,
             render_sql = TRUE,
             conn = NULL) {



            if (missing(names_from)) {

                    stop('argument "names_from" is missing, with no default')

            }

            names_from <- paste0("relative_", names_from)

            if (is.null(column)) {

                    column <- colnames(.data)[1]

            }

            output <<- leftJoinRelatives(.data = .data,
                                        athena_schema = athena_schema,
                                        id_column = column,
                                        whereLevelIn = whereLevelIn,
                                        whereLevelType = whereLevelType,
                                        render_sql = render_sql,
                                        conn = conn)



            #Binding output back
            output <-
                output %>%
                dplyr::select(-relative_type,
                              -min_levels_of_separation,
                              -max_levels_of_separation) %>%
                mergeStrip(into = "RelativeConcept",
                           has_prefix = "relative_",
                           !!names_from)

            final_output <-
            output %>%
                tidyr::pivot_wider(id_cols = !!column,
                                   names_from = !!names_from,
                                   values_from = RelativeConcept,
                                   values_fn = list(RelativeConcept = function(x) paste(unique(x)[1:250] %>%
                                                                                               centipede::no_na(), collapse = "\n"))) %>%
                dplyr::mutate_all(substring, 1, 25000) %>%
                dplyr::mutate_at(vars(!!column),
                                 as.integer)


            if (include_count) {

                final_output_count <-
                    output %>%
                    tidyr::pivot_wider(id_cols = !!column,
                                       names_from = !!names_from,
                                       values_from = RelativeConcept,
                                       values_fn = list(RelativeConcept = function(x) length(unique(x)))) %>%
                    dplyr::rename_at(vars(!(!!column)),
                                     function(x) paste0(x, " Count"))

                final_output <-
                    dplyr::left_join(final_output,
                                     final_output_count,
                                     by = column)



            }

            dplyr::left_join(.data,
                             final_output,
                             by = column)

    }
