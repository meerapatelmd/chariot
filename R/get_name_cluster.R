#' @title
#' Get a Name Cluster
#'
#' @description
#' A `Name Cluster` is a character vector of all the unique labels associated
#' with a concept, including labels derived from `Maps to` relationships from
#' the Concept Relationship table.
#'
#' @param concept_obj May be an integer or a `concept` class object.
#'
#' @export
#' @rdname get_name_cluster
#' @example inst/example/get_name_cluster.R

get_name_cluster <-
  function(concept_obj,
           vocab_schema = "omop_vocabulary",
           conn,
           conn_fun = "connectAthena()",
           cache_only = FALSE,
           skip_cache = FALSE,
           override_cache = FALSE,
           render_sql = TRUE,
           verbose = TRUE,
           sleepTime = 1) {

    # concept_id <- 1112807

    if (class(concept_obj) == "concept") {
      concept_id <- concept_obj@concept_id
    } else {
      concept_id <- concept_obj
      concept_obj <-
      get_concept(concept_id = concept_id,
                  vocab_schema = vocab_schema,
                  conn = conn,
                  conn_fun = conn_fun,
                  cache_only = cache_only,
                  skip_cache = skip_cache,
                  override_cache = override_cache,
                  render_sql = render_sql,
                  verbose = verbose,
                  sleepTime = sleepTime)
    }


    output <-
    c(concept_name = concept_obj@concept_name,
      concept_synonym_name = concept_obj@concept_synonym_names,
      maps_to_concept_name = concept_obj@maps_to_concept_names) %>%
      purrr::map(strsplit, split = "[|]{1}") %>%
      purrr::map(unlist) %>%
      unlist()

    output[!(output %in% c(""))]


  }
