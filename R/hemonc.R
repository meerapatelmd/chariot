#' @title
#' Extract HemOnc Regimens by Component String Match
#'
#' @description
#' This function uses the grepl function on the HemOnc Concept Names and naming patterns to return HemOnc Regimens based on a single Component and total number of Components in the source regimen.
#'
#' @inheritParams queryAthena
#' @param components Character vector of length 1 or greater of components that comprise the regimen.
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

grep_hemonc_regimens <-
    function(conn,
             components,
             check_validity = TRUE,
             component_count = NULL,
             vocabSchema = "omop_vocabulary",
             cache_only = FALSE,
             skip_cache = FALSE,
             override_cache = FALSE,
             render_sql = TRUE,
             verbose = TRUE,
             sleepTime = 1) {

        # component <- "trastuzumab"
        # component_count <- NULL
        # omop <- FALSE
        # vocabSchema <- "omop_vocabulary"

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
                                FROM @vocabSchema.concept c
                                LEFT JOIN @vocabSchema.concept_synonym cs
                                ON cs.concept_id = c.concept_id
                                WHERE
                                    c.invalid_reason IS NULL
                                    AND c.vocabulary_id = 'HemOnc'
                                    AND c.concept_class_id = 'Component'
                        )

                        SELECT DISTINCT c.*
                        FROM components comp
                        LEFT JOIN @vocabSchema.concept c
                        ON c.concept_id = comp.concept_id
                        WHERE LOWER(comp.concept_name) LIKE LOWER('%@component%')
                        ",
                            vocabSchema = vocabSchema,
                            component = components[i]
                    )


                output <-
                    queryAthena(sql_statement = sql_statement,
                                conn = conn,
                                cache_only = cache_only,
                                skip_cache = skip_cache,
                                override_cache = override_cache,
                                cache_resultset = cache_resultset,
                                render_sql = render_sql,
                                verbose = verbose,
                                sleepTime = sleepTime)

                if (nrow(output) == 0) {

                        stop(sprintf("'%s' is not a valid HemOnc Component", components[i]))

                }
            }


        }


        if (verbose) {

                cli::cat_line()
                cli::cli_rule(left = "Querying")

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
                                        FROM @vocabSchema.concept c
                                        LEFT JOIN @vocabSchema.concept_synonym cs
                                        ON cs.concept_id = c.concept_id
                                        WHERE
                                            c.invalid_reason IS NULL
                                            AND c.vocabulary_id = 'HemOnc'
                                            AND c.concept_class_id = 'Regimen'
                                )

                                SELECT DISTINCT c.*
                                FROM regimens r
                                LEFT JOIN @vocabSchema.concept c
                                ON c.concept_id = r.concept_id
                                WHERE LOWER(r.concept_name) LIKE LOWER('%@component%')
                                ",
                                vocabSchema = vocabSchema,
                                component = components[i])


                output[[i]] <-
                        queryAthena(sql_statement = sql_statement,
                                    conn = conn,
                                    cache_only = cache_only,
                                    skip_cache = skip_cache,
                                    override_cache = override_cache,
                                    cache_resultset = cache_resultset,
                                    render_sql = render_sql,
                                    verbose = verbose,
                                    sleepTime = sleepTime)


        }

        output <-
            output %>%
            purrr::reduce(dplyr::inner_join, by = c("concept_id", "concept_name", "domain_id", "vocabulary_id", "concept_class_id", "standard_concept", "concept_code", "valid_start_date", "valid_end_date", "invalid_reason"))


        if (is.null(component_count)) {

                output %>%
                    rubix::arrange_by_nchar(col = concept_name)

        } else if (component_count == 1) {


                output %>%
                    rubix::filter_at_grepl(concept_name,
                                           grepl_phrase = " monotherapy",
                                           evaluates_to = TRUE,
                                           ignore.case = TRUE) %>%
                    rubix::arrange_by_nchar(col = concept_name)


        } else if (component_count == 2) {

                output %>%
                    rubix::filter_at_grepl(concept_name,
                                           grepl_phrase = " and ",
                                           evaluates_to = TRUE,
                                           ignore.case = TRUE) %>%
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

            max <-  1 + (output %>%
                        dplyr::transmute(comma_count = n_comma(concept_name)) %>%
                        unlist() %>%
                        max(na.rm = TRUE))
            warning('"component_count" max is: ', max,  ' returning unfiltered output')
            output

        }
    }


#' Normalize To HemOnc Components
#' @description This function takes a mixture of HemOnc Regimen and HemOnc Component Concepts and returns all the unique HemOnc Components associated with the input combination.
#' @param hemonc_concept_ids HemOnc Vocabulary Concept Ids of either Regimen or Component concept classes.
#' @import rubix
#' @import dplyr
#' @export



normalizeToHemOncComponents <-
    function(hemonc_concept_ids,
             schema = NULL) {

        # If any of the concept_ids are regimens, to get their antineoplastic components
        input_concept <- query_concept_id(hemonc_concept_ids)

        qa <- input_concept %>%
            rubix::filter_for(filter_col = concept_class_id,
                              inclusion_vector = c("Regimen",
                                                   "Component"),
                              invert = TRUE)

        if (nrow(qa)) {
            qaNormalizeToHemOncComponents <<- qa
            stop('input concept ids are not Regimen or Components. See qaNormalizeToHemOncComponents for more details.')
        }

        input_regimens <- input_concept %>%
            dplyr::filter(concept_class_id == "Regimen")

        input_components <- input_concept %>%
            dplyr::filter(concept_class_id == "Component")


        if (nrow(input_regimens)) {


            component_concept_ids <-
                c(input_components$concept_id,
                  queryHemOncRegToAntineo(regimen_concept_ids = input_regimens$concept_id,
                                          schema = schema) %>%
                      dplyr::select(has_antineoplastic_concept_id) %>%
                      unlist() %>%
                      unname())


        } else {

            component_concept_ids <- input_components$concept_id

        }

        return(unique(component_concept_ids))
    }
