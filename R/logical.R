#' QA to make sure the concept_id exists in OMOP
#' @export
#' @rdname concept_id_exists


concept_id_exists <-
  function(concept_id,
           vocab_schema,
           conn = NULL,
           cache_only = FALSE,
           skip_cache = FALSE,
           override_cache = FALSE,
           render_sql = FALSE,
           verbose = FALSE,
           sleepTime = 1) {
    x <- lookup_concept_id(
      concept_id = concept_id,
      vocab_schema = vocab_schema,
      conn = conn,
      cache_only = cache_only,
      skip_cache = skip_cache,
      override_cache = override_cache,
      render_sql = render_sql,
      verbose = verbose,
      sleepTime = sleepTime
    )

    if (nrow(x) > 0) {
      TRUE
    } else {
      FALSE
    }
  }
