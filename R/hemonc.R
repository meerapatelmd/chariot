#' @title
#' Extract HemOnc Regimens by Component String Match
#'
#' @description
#' This function uses the grepl function on the HemOnc Concept Names and naming patterns to return HemOnc Regimens based on a single Component and total number of Components in the source regimen.
#'
#' @inheritParams queryAthena
#' @param component Character vector of length 1 or greater of components that comprise the regimen.
#' @param check_validity If TRUE, a query is run to comfirm that there is a HemOnc Component that has an exact string match to each of the components provided.
#' @param component_count If NULL or the component_count is larger than the maximum number of components possible for all Regimens with a positive string match to the `component` parameter, the unfiltered result of the initial query for `components` is returned.
#' @seealso
#'  \code{\link[cli]{cli_rule}},\code{\link[cli]{cat_line}}
#'  \code{\link[SqlRender]{render}}
#'  \code{\link[purrr]{reduce}}
#'  \code{\link[dplyr]{mutate-joins}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{filter}}
#'  \code{\link[rubix]{arrange_by_nchar}},\code{\link[rubix]{filter_at_grepl}}
#' @rdname grep_hemonc_regimens
#' @export
#' @importFrom cli cli_rule cat_line
#' @importFrom SqlRender render
#' @importFrom purrr reduce
#' @importFrom dplyr inner_join mutate filter transmute
#' @importFrom rubix arrange_by_nchar filter_at_grepl

ho_grep_regimens <-
  function(conn,
           conn_fun,
           components,
           check_validity = TRUE,
           component_count = NULL,
           vocab_schema = "omop_vocabulary",
           cache_only = FALSE,
           skip_cache = FALSE,
           override_cache = FALSE,
           render_sql = TRUE,
           verbose = TRUE,
           sleepTime = 1) {

    # component <- "trastuzumab"
    # component_count <- NULL
    # omop <- FALSE
    # vocab_schema <- "omop_vocabulary"

    if (check_validity) {
      if (verbose) {
        cli::cli_rule(left = "Checking Validity")
      }


      for (i in seq_along(components)) {
        sql_statement <-
          SqlRender::render(
            "
                        WITH components AS (
                                SELECT DISTINCT
                                        c.concept_id,
                                        cs.concept_synonym_name AS concept_name
                                FROM @vocab_schema.concept c
                                LEFT JOIN @vocab_schema.concept_synonym cs
                                ON cs.concept_id = c.concept_id
                                WHERE
                                    c.invalid_reason IS NULL
                                    AND c.vocabulary_id = 'HemOnc'
                                    AND c.concept_class_id = 'Component'
                        )

                        SELECT DISTINCT c.*
                        FROM components comp
                        LEFT JOIN @vocab_schema.concept c
                        ON c.concept_id = comp.concept_id
                        WHERE LOWER(comp.concept_name) LIKE '%@component%'
                        ",
            vocab_schema = vocab_schema,
            component = tolower(components[i])
          )


        output <-
          queryAthena(
            sql_statement = sql_statement,
            conn = conn,
            conn_fun = conn_fun,
            cache_only = cache_only,
            skip_cache = skip_cache,
            override_cache = override_cache,
            cache_resultset = cache_resultset,
            render_sql = render_sql,
            verbose = verbose,
            sleepTime = sleepTime
          )

        if (nrow(output) == 0) {
          cli::cli_alert_danger(components[i])
          stop(sprintf("'%s' is not a valid HemOnc Component", components[i]))
        } else {
          cli::cli_alert_success(components[i])
        }
      }
    }


    if (verbose) {
      cli::cat_line()
      cli::cli_rule(left = "Query")
    }

    output <- list()
    for (i in seq_along(components)) {
      sql_statement <-
        SqlRender::render(
          "
                                WITH regimens AS (
                                        SELECT DISTINCT
                                                c.concept_id,
                                                cs.concept_synonym_name AS concept_name
                                        FROM @vocab_schema.concept c
                                        LEFT JOIN @vocab_schema.concept_synonym cs
                                        ON cs.concept_id = c.concept_id
                                        WHERE
                                            c.invalid_reason IS NULL
                                            AND c.vocabulary_id = 'HemOnc'
                                            AND c.concept_class_id = 'Regimen'
                                )

                                SELECT DISTINCT c.*
                                FROM regimens r
                                LEFT JOIN @vocab_schema.concept c
                                ON c.concept_id = r.concept_id
                                WHERE LOWER(r.concept_name) LIKE LOWER('%@component%')
                                ",
          vocab_schema = vocab_schema,
          component = components[i]
        )


      output[[i]] <-
        queryAthena(
          sql_statement = sql_statement,
          conn = conn,
          conn_fun = conn_fun,
          cache_only = cache_only,
          skip_cache = skip_cache,
          override_cache = override_cache,
          cache_resultset = cache_resultset,
          render_sql = render_sql,
          verbose = verbose,
          sleepTime = sleepTime
        )
    }

    output <-
      output %>%
      purrr::reduce(dplyr::inner_join,
        by = c(
          "concept_id",
          "concept_name",
          "domain_id",
          "vocabulary_id",
          "concept_class_id",
          "standard_concept",
          "concept_code",
          "valid_start_date",
          "valid_end_date",
          "invalid_reason"
        )
      )


    if (is.null(component_count)) {
      output %>%
        rubix::arrange_by_nchar(col = concept_name)
    } else if (component_count == 1) {
      output %>%
        rubix::filter_at_grepl(concept_name,
          grepl_phrase = " monotherapy",
          evaluates_to = TRUE,
          ignore.case = TRUE
        ) %>%
        rubix::arrange_by_nchar(col = concept_name)
    } else if (component_count == 2) {
      output %>%
        rubix::filter_at_grepl(concept_name,
          grepl_phrase = " and ",
          evaluates_to = TRUE,
          ignore.case = TRUE
        ) %>%
        rubix::arrange_by_nchar(col = concept_name)
    } else if (component_count == 3) {
      output %>%
        dplyr::mutate(comma_count = n_comma(concept_name)) %>%
        dplyr::filter(comma_count == 2) %>%
        rubix::arrange_by_nchar(col = concept_name)
    } else if (component_count == 4) {
      output %>%
        dplyr::mutate(comma_count = n_comma(concept_name)) %>%
        dplyr::filter(comma_count == 3) %>%
        rubix::arrange_by_nchar(col = concept_name)
    } else {
      max <- 1 + (output %>%
        dplyr::transmute(comma_count = n_comma(concept_name)) %>%
        unlist() %>%
        max(na.rm = TRUE))
      warning('"component_count" max is: ', max, " returning unfiltered output")
      output
    }
  }



#' Query a HemOnc Regimen's 'Has antineoplastic' Relationship
#' @export

ho_lookup_antineoplastics <-
  function(regimen_concept_ids,
           vocab_schema = NULL,
           check_validity = TRUE,
           conn,
           cache_only = FALSE,
           skip_cache = FALSE,
           override_cache = FALSE,
           render_sql = FALSE,
           verbose = FALSE,
           sleepTime = 1) {
    if (check_validity) {
      if (verbose) {
        cli::cli_rule(left = "Checking Validity")
      }

      sql_statement <-
        SqlRender::render(
          "
                            SELECT *
                            FROM @vocab_schema.concept c
                            WHERE c.concept_id IN (@regimen_concept_ids)
                                    AND c.invalid_reason IS NULL
                                    AND c.concept_class_id = 'Regimen'
                                    AND c.vocabulary_id = 'HemOnc'
                            ",
          vocab_schema = vocab_schema,
          regimen_concept_ids = regimen_concept_ids
        )

      output <- queryAthena(
        sql_statement = sql_statement,
        conn = conn,
        cache_only = cache_only,
        skip_cache = skip_cache,
        override_cache = override_cache,
        render_sql = render_sql,
        verbose = verbose,
        sleepTime = sleepTime
      )

      if (nrow(output) != length(regimen_concept_ids)) {
        invalid_ids <- regimen_concept_ids[!(regimen_concept_ids %in% output$concept_id)]
        stop("Invalid concept ids: %s", paste(invalid_ids, collapse = ", "))
      }
    }


    if (verbose) {
      cli::cat_rule(left = "Querying")
    }

    sql_statement <-
      SqlRender::render(
        "
                        SELECT
                            cr.concept_id_1 AS regimen_concept_id,
                            c.concept_id AS has_antineoplastic_concept_id,
                            c.concept_name AS has_antineoplastic_concept_name
                        FROM @schema.concept_relationship cr
                        LEFT JOIN @schema.concept c
                        ON c.concept_id = cr.concept_id_2
                        WHERE cr.concept_id_1 IN (@regimen_concept_ids)
                                AND cr.relationship_id = 'Has antineoplastic'
                                AND c.concept_class_id = 'Component'
                                AND c.vocabulary_id = 'HemOnc';
                        ",
        regimen_concept_ids = regimen_concept_ids
      )

    queryAthena(
      sql_statement = sql_statement,
      conn = conn,
      cache_only = cache_only,
      skip_cache = skip_cache,
      override_cache = override_cache,
      render_sql = render_sql,
      verbose = verbose,
      sleepTime = sleepTime
    )
  }

#' Normalize To HemOnc Components
#' @description This function takes a mixture of HemOnc Regimen and HemOnc Component Concepts and returns all the unique HemOnc Components associated with the input combination.
#' @param hemonc_concept_ids HemOnc Vocabulary Concept Ids of either Regimen or Component concept classes.
#' @import rubix
#' @import dplyr
#' @export


deconstruct_hemonc_ids <-
  function(hemonc_concept_ids,
           schema = NULL) {

    # If any of the concept_ids are regimens, to get their antineoplastic components
    input_concept <- queryConceptId(hemonc_concept_ids)

    qa <- input_concept %>%
      dplyr::filter(!(concept_class_id %in% c("Regimen", "Component")))

    if (nrow(qa)) {
      stop(sprintf("concept id arguments %s are not Regimen or Components.", paste(qa$concept_id, collapse = ", ")))
    }

    input_regimens <- input_concept %>%
      dplyr::filter(concept_class_id == "Regimen")
    input_components <- input_concept %>%
      dplyr::filter(concept_class_id == "Component")


    if (nrow(input_regimens)) {
      component_concept_ids <-
        c(
          input_components$concept_id,
          queryHemOncRegToAntineo(
            regimen_concept_ids = input_regimens$concept_id,
            schema = schema
          ) %>%
            dplyr::select(has_antineoplastic_concept_id) %>%
            unlist() %>%
            unname()
        )
    } else {
      component_concept_ids <- input_components$concept_id
    }

    return(unique(component_concept_ids))
  }



#' @title
#' Look up a HemOnc Regimen
#' @description
#' Look up a HemOnc Regimen that Corresponds to the set of HemOnc Components provided by Id. This function takes a set list of HemOnc Components and finds all the HemOnc Regimens that contain these Components in the CONCEPT_RELATIONSHIP table. The resultset is then filtered for the HemOnc Regimens that have the same exact number of Components as the length of the `component_concept_ids` parameter.
#' @param component_concept_ids integer vector of length 1 or greater of all the Component Concept Ids belonging to any regimen to map to a HemOnc Regimen.
#' @param ... Additional arguments passed to the query_athena function.
#' @import magrittr
#' @import dplyr
#' @export

ho_lookup_regimen <-
  function(component_concept_ids,
           schema = NULL,
           vocab_schema,
           writeSchema,
           conn = NULL,
           cache_only = FALSE,
           skip_cache = FALSE,
           override_cache = FALSE,
           render_sql = FALSE,
           verbose = FALSE,
           sleepTime = 1) {


    # component_concept_ids <- c(35803211, 35803383)

    # QA that all component concept ids are Components
    qa <-
      queryAthena(
        sql_statement =
          SqlRender::render(
            "SELECT *
                             FROM @vocab_schema.concept c
                             WHERE c.concept_id IN (@component_concept_ids)
                                AND c.invalid_reason IS NULL
                                AND c.concept_class_id <> 'Component';",
            component_concept_ids = component_concept_ids,
            vocab_schema = vocab_schema
          )
      )


    if (nrow(qa) > 0) {
      qa <-
        qa %>%
        mergeLabel(
          into = "Label",
          remove = FALSE
        ) %>%
        dplyr::select(Label) %>%
        unlist() %>%
        paste(collapse = ", ")

      stop(sprintf("Not all `component_concept_ids` are HemOnc Components: %s", qa))
    }


    # Query Athena DB for all Regimens associated with the inputted Component Concept Ids

    output <- list()
    for (i in seq_along(component_concept_ids)) {
      output[[i]] <-
        queryAthena(
          conn = conn,
          sql_statement =
            SqlRender::render(
              "WITH component_regimens AS (
                                        SELECT cr.concept_id_2 AS regimen_i_concept_id
                                        FROM @vocab_schema.concept c
                                        LEFT JOIN @vocab_schema.concept_relationship cr
                                        ON cr.concept_id_1 = c.concept_id
                                        WHERE c.concept_id IN (@component_concept_id)
                                            AND cr.relationship_id = 'Antineoplastic of'
                                            AND cr.invalid_reason IS NULL
                                ),
                                regimens AS (
                                       SELECT DISTINCT cr.concept_id_1 AS regimen_concept_id
                                       FROM @vocab_schema.concept c
                                       LEFT JOIN @vocab_schema.concept_relationship cr
                                       ON c.concept_id = cr.concept_id_1
                                       WHERE c.vocabulary_id = 'HemOnc'
                                       AND c.invalid_reason IS NULL
                                       AND c.concept_class_id = 'Regimen'
                                       AND cr.invalid_reason IS NULL
                                       AND cr.relationship_id = 'Has antineoplastic'
                                       GROUP BY cr.concept_id_1
                                       HAVING COUNT(cr.concept_id_2) = @component_count
                                )

                                SELECT t.*
                                FROM component_regimens cr
                                INNER JOIN regimens t
                                ON t.regimen_concept_id = cr.regimen_i_concept_id
                                ;
                                ",
              vocab_schema = vocab_schema,
              component_count = length(component_concept_ids),
              component_concept_id = component_concept_ids[i]
            )
        )
    }

    output %>%
      purrr::reduce(dplyr::inner_join, by = "regimen_concept_id") %>%
      unlist()
  }


#' Find HemOnc Regimen by Components
#' @description This function takes a set list of HemOnc Components and finds all the HemOnc Regimens that contain these Components in the CONCEPT_RELATIONSHIP table. The resultset is then filtered for the HemOnc Regimens that have the same exact number of Components as the length of the `component_concept_ids` parameter.
#' @param component_concept_ids integer vector of length 1 or greater of all the Component Concept Ids belonging to any regimen to map to a HemOnc Regimen.
#' @param ... Additional arguments passed to the query_athena function.
#' @import magrittr
#' @import dplyr
#' @export

queryHemOncCompToReg <-
  function(component_concept_ids,
           schema = NULL,
           conn = NULL,
           cache_only = FALSE,
           skip_cache = FALSE,
           override_cache = FALSE,
           render_sql = FALSE,
           verbose = FALSE,
           sleepTime = 1) {
    .Deprecated("ho_lookup_regimen")

    # For inputs that are actually regimens, a new set of components is derived.
    component_concept_ids <-
      normalizeToHemOncComponents(
        hemonc_concept_ids = component_concept_ids,
        schema = schema
      )

    # Get input component count to filter HemOnc Regimens based on their own component_counts
    input_component_count <- length(component_concept_ids)

    # If any of the concept_ids are regimens, to get their antineoplastic components
    input_concept <-
      queryConceptId(component_concept_ids,
        schema = schema
      )

    qa <- input_concept %>%
      rubix::filter_for(
        filter_col = concept_class_id,
        inclusion_vector = c(
          "Regimen",
          "Component"
        ),
        invert = TRUE
      )

    if (nrow(qa)) {
      qaHemOncCompToReg <<- qa
      stop("input concept ids are not Regimen or Components. See qaHemOncCompToReg for more details.")
    }


    # Query Athena DB for all Regimens associated with the inputted Component Concept Ids
    sql_statement <-
      renderHemOncCompToReg(
        component_concept_ids = component_concept_ids,
        schema = schema
      )

    Regimens <-
      queryAthena(
        sql_statement = sql_statement,
        conn = conn,
        cache_only = cache_only,
        skip_cache = skip_cache,
        override_cache = override_cache,
        render_sql = render_sql,
        verbose = verbose,
        sleepTime = sleepTime
      )

    # Query again to get all of the "Has antineoplastic" relationships to HemOnc Components these Regimens have
    HasAntineoplastics <- queryHemOncRegToAntineo(
      regimen_concept_ids = Regimens$regimen_concept_id,
      schema = schema
    )



    # Getting the number of unique HemOnc Components associated with each of the HemOnc Regimens found and then filtering for the length of the input component_concept_ids vector
    HasAntineoplastics2 <-
      HasAntineoplastics %>%
      dplyr::group_by(regimen_concept_id) %>%
      dplyr::summarize(has_antineoplastic_count = length(unique(has_antineoplastic_concept_id)), .groups = "drop") %>%
      dplyr::filter(has_antineoplastic_count == input_component_count) %>%
      dplyr::ungroup() %>%
      dplyr::select(regimen_concept_id) %>%
      left_join_concept() %>%
      dplyr::select(-any_of("regimen_concept_id")) %>%
      rubix::rename_all_prefix("regimen_")

    # If only 1 or less rows, the function is complete. Otherwise, the outputs need to be filtered another time since now we have all the Regimens that have the exact component count match as the input and have at least 1 of the input components, but does not necessarily have all the components
    #
    if (nrow(HasAntineoplastics2) <= 1) {
      return(HasAntineoplastics2)
    } else {
      HasAntineoplastics3 <-
        HasAntineoplastics %>%
        dplyr::select(
          regimen_concept_id,
          has_antineoplastic_concept_id
        ) %>%
        dplyr::group_by(regimen_concept_id) %>%
        dplyr::summarise_at(
          vars(has_antineoplastic_concept_id),
          list(
            has_all_components = ~ all(. %in% component_concept_ids),
            component_count = ~ length(unique(.))
          )
        ) %>%
        dplyr::filter(
          has_all_components == TRUE,
          component_count == input_component_count
        ) %>%
        dplyr::select(regimen_concept_id) %>%
        left_join_concept(include_synonyms = F) %>%
        dplyr::select(-starts_with("regimen")) %>%
        rubix::rename_all_prefix("regimen_")

      return(HasAntineoplastics3)
    }
  }




#' Query a HemOnc Regimen's 'Has antineoplastic' Relationship
#' @export

queryHemOncRegToAntineo <-
  function(regimen_concept_ids,
           schema = NULL,
           conn = NULL,
           cache_only = FALSE,
           skip_cache = FALSE,
           override_cache = FALSE,
           render_sql = FALSE,
           verbose = FALSE,
           sleepTime = 1) {
    .Deprecated("ho_lookup_antineoplastics")

    sql_statement <-
      renderHemOncRegToAntineoplastics(
        regimen_concept_ids = regimen_concept_ids,
        schema = schema
      )

    queryAthena(
      sql_statement = sql_statement,
      conn = conn,
      cache_only = cache_only,
      skip_cache = skip_cache,
      override_cache = override_cache,
      render_sql = render_sql,
      verbose = verbose,
      sleepTime = sleepTime
    )
  }
