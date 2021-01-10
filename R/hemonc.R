#' @title
#' Extract HemOnc Regimens by Component String Match
#'
#' @description
#' This function uses the grepl function on the HemOnc Concept Names and naming
#' patterns to return HemOnc Regimens based on a single Component and total
#' number of Components in the source regimen.
#'
#' @inheritParams queryAthena
#' @param ... Characters of length 1 or greater of components that comprise the
#' regimen.
#' @param check_validity If TRUE, a query is run to comfirm that there is a
#' HemOnc Component that has an exact string match to each of the components
#' provided.
#' @param component_count If NULL or the component_count is larger than the m
#' aximum number of components possible for all Regimens with a positive string
#' match to the `component` parameter, the unfiltered result of the initial
#' query for `components` is returned.
#' @seealso
#'  \code{\link[cli]{cli_rule}},\code{\link[cli]{cat_line}}
#'  \code{\link[SqlRender]{render}}
#'  \code{\link[purrr]{reduce}}
#'  \code{\link[dplyr]{mutate-joins}},\code{\link[dplyr]{mutate}},
#'  \code{\link[dplyr]{filter}}
#'  \code{\link[rubix]{arrange_by_nchar}},\code{\link[rubix]{filter_at_grepl}}
#' @rdname ho_grep_regimens
#' @export
#' @importFrom cli cli_rule cat_line
#' @importFrom SqlRender render
#' @importFrom purrr reduce
#' @importFrom dplyr inner_join mutate filter transmute
#' @importFrom rubix arrange_by_nchar filter_at_grepl

ho_grep_regimens <-
  function(...,
           conn,
           conn_fun = "connectAthena()",
           check_validity = TRUE,
           component_count = NULL,
           vocab_schema = "omop_vocabulary",
           cache_only = FALSE,
           skip_cache = FALSE,
           override_cache = FALSE,
           render_sql = TRUE,
           verbose = TRUE,
           sleepTime = 1) {

    n_comma <-
      function(string) {
        nchar(
        stringr::str_remove_all(string = string,
                                pattern = "[^,]")
        )
      }
    #
    if (missing(...)) {
      stop("components required")
    }


    components <- rlang::list2(...)
    components <- unlist(components)


    if (is.null(component_count)) {
      component_count <- length(components)
    }

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


   if (component_count == 1) {
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



#' @title
#' Query a HemOnc Regimen's 'Has antineoplastic' Relationship
#' @seealso
#'  \code{\link[cli]{cli_rule}},\code{\link[cli]{cat_line}}
#'  \code{\link[SqlRender]{render}}
#' @rdname ho_lookup_antineoplastics
#' @export
#' @importFrom cli cli_rule cat_rule
#' @importFrom SqlRender render

ho_lookup_antineoplastics <-
  function(...,
           vocab_schema = "omop_vocabulary",
           check_validity = TRUE,
           conn,
           cache_only = FALSE,
           skip_cache = FALSE,
           override_cache = FALSE,
           render_sql = FALSE,
           verbose = FALSE,
           sleepTime = 1) {

    regimen_concept_objs <- unlist(rlang::list2(...))

    regimen_concept_ids <- vector()
    for (i in seq_along(regimen_concept_objs)) {
      regimen_concept_obj <- regimen_concept_objs[[i]]
      if (class(regimen_concept_obj) == "concept") {
        regimen_concept_ids <-
          c(regimen_concept_ids,
            regimen_concept_obj@concept_id)
      } else {
        regimen_concept_ids <-
          c(regimen_concept_ids,
            regimen_concept_obj)
      }
    }

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
        regimen_concept_ids = regimen_concept_ids,
        schema = vocab_schema
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



#' @title
#' Look up a HemOnc Regimen
#' @description
#' Look up a HemOnc Regimen that Corresponds to the set of HemOnc Components provided by Id. This function takes a set list of HemOnc Components and finds all the HemOnc Regimens that contain these Components in the CONCEPT_RELATIONSHIP table. The resultset is then filtered for the HemOnc Regimens that have the same exact number of Components as the length of the `component_concept_ids` parameter.
#' @param component_concept_ids integer vector of length 1 or greater of all the Component Concept Ids belonging to any regimen to map to a HemOnc Regimen.
#' @param ... Additional arguments passed to the query_athena function.
#' @seealso
#'  \code{\link[SqlRender]{render}}
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{mutate-joins}}
#'  \code{\link[purrr]{reduce}}
#' @rdname ho_lookup_regimen
#' @export
#' @importFrom SqlRender render
#' @importFrom dplyr select inner_join
#' @importFrom purrr reduce

ho_lookup_regimen <-
  function(...,
           schema = NULL,
           vocab_schema = "omop_vocabulary",
           conn = NULL,
           cache_only = FALSE,
           skip_cache = FALSE,
           override_cache = FALSE,
           render_sql = FALSE,
           verbose = FALSE,
           sleepTime = 1) {


    # component_concept_ids <- c(35803211, 35803383)
   component_concept_objs <- unlist(rlang::list2(...))

    component_concept_ids <- vector()
    for (i in seq_along(component_concept_objs)) {
      component_concept_obj <- component_concept_objs[[i]]
      if (class(component_concept_obj) == "concept") {
        component_concept_ids <-
          c(component_concept_ids,
            component_concept_obj@concept_id)
      } else {
        component_concept_ids <-
          c(component_concept_ids,
            component_concept_obj)
      }
    }

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

    regimen_concept_ids <-
    output %>%
      purrr::reduce(dplyr::inner_join, by = "regimen_concept_id") %>%
      unlist() %>%
      unname()

    regimens <-
    ho_lookup_antineoplastics(regimen_concept_ids = regimen_concept_ids,
                              vocab_schema = vocab_schema,
                              conn = conn)

    regimens2 <-
    join_on_concept_id(data = regimens,
                       column = "regimen_concept_id")

    regimens2 %>%
      select(regimen_concept_id,
             regimen_concept_name = concept_name,
             has_antineoplastic_concept_id,
             has_antineoplastic_concept_name)
  }

