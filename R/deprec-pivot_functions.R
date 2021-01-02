#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param .data PARAM_DESCRIPTION
#' @param column PARAM_DESCRIPTION, Default: NULL
#' @param athena_schema PARAM_DESCRIPTION, Default: 'public'
#' @param render_sql PARAM_DESCRIPTION, Default: TRUE
#' @param conn PARAM_DESCRIPTION, Default: NULL
#' @param names_from PARAM_DESCRIPTION
#' @param include_count PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[tidyr]{pivot_wider}}
#'  \code{\link[centipede]{no_na}}
#'  \code{\link[dplyr]{mutate_all}},\code{\link[dplyr]{select_all}},\code{\link[dplyr]{mutate-joins}},\code{\link[dplyr]{rename}}
#' @rdname pivotConcept2
#' @export
#' @importFrom tidyr pivot_wider
#' @importFrom centipede no_na
#' @importFrom dplyr mutate_at rename_at left_join rename

pivotConcept2 <-
  function(.data,
           column = NULL,
           athena_schema = "public",
           render_sql = TRUE,
           conn = NULL,
           names_from,
           include_count = TRUE) {
    if (missing(names_from)) {
      stop('argument "names_from" is missing, with no default')
    }

    names_from <- paste0(names_from, "_2")

    if (is.null(column)) {
      column <- colnames(.data)[1]
    }



    output <- leftJoinRelationship(
      .data = .data,
      column = column,
      athena_schema = athena_schema,
      render_sql = render_sql,
      conn = conn
    ) %>%
      mergeStrip(
        into = "Concept2",
        !!names_from,
        has_suffix = "_2"
      )



    final_output <-
      output %>%
      tidyr::pivot_wider(
        id_cols = concept_id_1,
        names_from = !!names_from,
        values_from = Concept2,
        values_fn = list(Concept2 = function(x) {
          paste(unique(x)[1:250] %>%
            centipede::no_na(),
          collapse = "\n"
          )
        })
      ) %>%
      dplyr::mutate_at(
        vars(concept_id_1),
        as.integer
      )


    if (include_count) {
      final_output_count <-
        output %>%
        tidyr::pivot_wider(
          id_cols = concept_id_1,
          names_from = !!names_from,
          values_from = Concept2,
          values_fn = list(Concept2 = function(x) length(unique(x)))
        ) %>%
        dplyr::rename_at(
          vars(!(concept_id_1)),
          function(x) paste0(x, " Count")
        )

      final_output <-
        dplyr::left_join(final_output,
          final_output_count,
          by = "concept_id_1"
        )
    }

    dplyr::left_join(
      .data %>%
        dplyr::rename(concept_id_1 = !!column),
      final_output
    ) %>%
      dplyr::rename(!!column := concept_id_1)
  }





#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param .data PARAM_DESCRIPTION
#' @param column PARAM_DESCRIPTION, Default: NULL
#' @param athena_schema PARAM_DESCRIPTION, Default: 'public'
#' @param max_levels_of_separation PARAM_DESCRIPTION, Default: TRUE
#' @param AND PARAM_DESCRIPTION, Default: TRUE
#' @param render_sql PARAM_DESCRIPTION, Default: TRUE
#' @param conn PARAM_DESCRIPTION, Default: NULL
#' @param include_count PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[dplyr]{arrange_all}},\code{\link[dplyr]{mutate_all}},\code{\link[dplyr]{bind}},\code{\link[dplyr]{select}},\code{\link[dplyr]{select_all}},\code{\link[dplyr]{mutate-joins}}
#'  \code{\link[tidyr]{pivot_wider}}
#'  \code{\link[centipede]{no_na}}
#' @rdname pivotLevel
#' @export
#' @importFrom dplyr arrange_at mutate_at bind_rows select mutate_all rename_at left_join
#' @importFrom tidyr pivot_wider
#' @importFrom centipede no_na
pivotLevel <-
  function(.data,
           column = NULL,
           athena_schema = "public",
           max_levels_of_separation = TRUE,
           AND = TRUE,
           render_sql = TRUE,
           conn = NULL,
           include_count = TRUE) {
    if (is.null(column)) {
      column <- colnames(.data)[1]
    }

    if (max_levels_of_separation) {
      levels_type <- "max_levels_of_separation"
    } else {
      levels_type <- "min_levels_of_separation"
    }


    output <- leftJoinRelatives(
      .data = .data,
      id_column = column,
      athena_schema = athena_schema,
      render_sql = render_sql,
      conn = conn
    )


    # Split A and D
    output <- split(
      output,
      output$relative_type
    )

    # Arrange descending from A then ascending from 0 to D
    output$A <-
      output$A %>%
      dplyr::arrange_at(
        vars(!!levels_type),
        function(x) desc(as.integer(x))
      ) %>%
      dplyr::mutate_at(
        vars(!!levels_type),
        function(x) paste0("A_", x)
      )


    output$D <-
      output$D %>%
      dplyr::arrange_at(
        vars(!!levels_type),
        function(x) as.integer(x)
      ) %>%
      dplyr::mutate_at(
        vars(!!levels_type),
        function(x) paste0("D_", x)
      )

    # Binding output back
    output <-
      output %>%
      dplyr::bind_rows() %>%
      mergeStrip(
        into = "RelativeConcept",
        has_prefix = "relative_"
      ) %>%
      dplyr::select(
        !!column,
        RelativeConcept,
        !!levels_type
      )

    final_output <-
      output %>%
      tidyr::pivot_wider(
        id_cols = !!column,
        names_from = !!levels_type,
        values_from = RelativeConcept,
        values_fn = list(RelativeConcept = function(x) {
          paste(unique(x)[1:250] %>%
            centipede::no_na(), collapse = "\n")
        })
      ) %>%
      dplyr::mutate_all(substring, 1, 25000) %>%
      dplyr::mutate_at(
        vars(!!column),
        as.integer
      )

    if (include_count) {
      final_output_count <-
        output %>%
        tidyr::pivot_wider(
          id_cols = !!column,
          names_from = !!levels_type,
          values_from = RelativeConcept,
          values_fn = list(RelativeConcept = function(x) length(unique(x)))
        ) %>%
        dplyr::rename_at(
          vars(!(!!column)),
          function(x) paste0(x, " Count")
        )

      final_output <-
        dplyr::left_join(
          final_output,
          final_output_count
        )
    }

    dplyr::left_join(.data,
      final_output,
      by = column
    )
  }





#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param .data PARAM_DESCRIPTION
#' @param column PARAM_DESCRIPTION, Default: NULL
#' @param athena_schema PARAM_DESCRIPTION, Default: 'public'
#' @param render_sql PARAM_DESCRIPTION, Default: TRUE
#' @param conn PARAM_DESCRIPTION, Default: NULL
#' @param include_count PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[tidyr]{pivot_wider}}
#'  \code{\link[centipede]{no_na}}
#'  \code{\link[dplyr]{mutate_all}},\code{\link[dplyr]{select_all}},\code{\link[dplyr]{mutate-joins}},\code{\link[dplyr]{rename}}
#' @rdname pivotRelationshipId
#' @export
#' @importFrom tidyr pivot_wider
#' @importFrom centipede no_na
#' @importFrom dplyr mutate_at rename_at left_join rename
pivotRelationshipId <-
  function(.data,
           column = NULL,
           athena_schema = "public",
           render_sql = TRUE,
           conn = NULL,
           include_count = TRUE) {
    if (is.null(column)) {
      column <- colnames(.data)[1]
    }



    output <- leftJoinRelationship(
      .data = .data,
      column = column,
      athena_schema = athena_schema,
      render_sql = render_sql,
      conn = conn
    ) %>%
      mergeStrip(
        into = "Concept2",
        has_suffix = "_2"
      )



    final_output <-
      output %>%
      tidyr::pivot_wider(
        id_cols = concept_id_1,
        names_from = relationship_id,
        values_from = Concept2,
        values_fn = list(Concept2 = function(x) {
          paste(unique(x)[1:250] %>%
            centipede::no_na(),
          collapse = "\n"
          )
        })
      ) %>%
      dplyr::mutate_at(
        vars(concept_id_1),
        as.integer
      )


    if (include_count) {
      final_output_count <-
        output %>%
        tidyr::pivot_wider(
          id_cols = concept_id_1,
          names_from = relationship_id,
          values_from = Concept2,
          values_fn = list(Concept2 = function(x) length(unique(x)))
        ) %>%
        dplyr::rename_at(
          vars(!(concept_id_1)),
          function(x) paste0(x, " Count")
        )

      final_output <-
        dplyr::left_join(final_output,
          final_output_count,
          by = "concept_id_1"
        )
    }

    dplyr::left_join(
      .data %>%
        dplyr::rename(concept_id_1 = !!column),
      final_output
    ) %>%
      dplyr::rename(!!column := concept_id_1)
  }





#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param .data PARAM_DESCRIPTION
#' @param athena_schema PARAM_DESCRIPTION, Default: 'public'
#' @param column PARAM_DESCRIPTION, Default: NULL
#' @param whereLevelIn PARAM_DESCRIPTION, Default: NULL
#' @param whereLevelType PARAM_DESCRIPTION, Default: NULL
#' @param names_from PARAM_DESCRIPTION
#' @param include_count PARAM_DESCRIPTION, Default: TRUE
#' @param render_sql PARAM_DESCRIPTION, Default: TRUE
#' @param conn PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{mutate_all}},\code{\link[dplyr]{select_all}},\code{\link[dplyr]{mutate-joins}}
#'  \code{\link[tidyr]{pivot_wider}}
#'  \code{\link[centipede]{no_na}}
#' @rdname pivotRelative
#' @export
#' @importFrom dplyr select mutate_at rename_at left_join
#' @importFrom tidyr pivot_wider
#' @importFrom centipede no_na
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

    output <<- leftJoinRelatives(
      .data = .data,
      athena_schema = athena_schema,
      id_column = column,
      whereLevelIn = whereLevelIn,
      whereLevelType = whereLevelType,
      render_sql = render_sql,
      conn = conn
    )



    # Binding output back
    output <-
      output %>%
      dplyr::select(
        -relative_type,
        -min_levels_of_separation,
        -max_levels_of_separation
      ) %>%
      mergeStrip(
        into = "RelativeConcept",
        has_prefix = "relative_",
        !!names_from
      )

    final_output <-
      output %>%
      tidyr::pivot_wider(
        id_cols = !!column,
        names_from = !!names_from,
        values_from = RelativeConcept,
        values_fn = list(RelativeConcept = function(x) {
          paste(unique(x)[1:250] %>%
            centipede::no_na(), collapse = "\n")
        })
      ) %>%
      dplyr::mutate_at(
        vars(!!column),
        as.integer
      )


    if (include_count) {
      final_output_count <-
        output %>%
        tidyr::pivot_wider(
          id_cols = !!column,
          names_from = !!names_from,
          values_from = RelativeConcept,
          values_fn = list(RelativeConcept = function(x) length(unique(x)))
        ) %>%
        dplyr::rename_at(
          vars(!(!!column)),
          function(x) paste0(x, " Count")
        )

      final_output <-
        dplyr::left_join(final_output,
          final_output_count,
          by = column
        )
    }

    dplyr::left_join(.data,
      final_output,
      by = column
    )
  }
