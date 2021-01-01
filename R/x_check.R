#' @title
#' Check Concept Id
#'
#' @description
#' Check if a `concept_id` is a valid integer.
#'
#' @export
#' @rdname check_concept_id
#' @importFrom cli cli_alert_success cli_alert_danger
#' @example inst/example/check.R

check_concept_id <-
  function(concept_id) {
    if (!is.na(concept_id)) {

      concept_id_int <-
        tryCatch(
          expr = suppressWarnings(as.integer(concept_id)),
          error = function(e) NA_integer_
        )

      if (is.na(concept_id_int)) {
        cli::cli_alert_danger("Concept Id is not a valid integer")
      } else {
        cli::cli_alert_success("Concept Id is a valid integer")
      }
    } else {
      cli::cli_alert_danger("Concept Id is NA")
    }
  }


#' @title
#' Check if a Concept Class Object Belongs to a Vocabulary
#' @seealso
#'  \code{\link[cli]{cli_alert}}
#' @rdname check_vocab_id
#' @export
#' @importFrom cli cli_alert_danger cli_alert_success
#' @example inst/example/check.R

check_vocab_id <-
  function(conn,
           conn_fun = "connectAthena()",
           concept_obj,
           vocab_schema = "omop_vocabulary",
           vocabulary_id,
           concept_class_id,
           cache_only = FALSE,
           skip_cache = FALSE,
           override_cache = FALSE,
           render_sql = FALSE,
           verbose = FALSE,
           sleepTime = 1) {
    if (class(concept_obj) != "concept") {

      concept_id <- concept_obj
      output <-
        get_concept(
          concept_id = concept_id,
          vocab_schema = vocab_schema,
          conn = conn,
          conn_fun = conn_fun,
          cache_only = cache_only,
          skip_cache = skip_cache,
          override_cache = override_cache,
          render_sql = render_sql,
          verbose = verbose,
          sleepTime = sleepTime
        )
    } else {
      output <- concept_obj
    }

    if (!is.null(output)) {
      if (!identical(vocabulary_id, output@vocabulary_id)) {
        cli::cli_alert_danger("Vocabulary Id is not {vocabulary_id}")
      } else {
        cli::cli_alert_success("Vocabulary Id is {vocabulary_id}")
      }
    }
  }
