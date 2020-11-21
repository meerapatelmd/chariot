#' @title Query the Athena Postgres Database
#' @description
#' By default, this function queries a local database named "Athena". If a connection object is passed into the function, the database of the connection object is queried instead. The caching feature is only available when using the built-in connection to Athena.
#'
#' @param sql_statement         SQL query
#' @param cache_only            Loads from the cache and does not query the database. A NULL object is returned if a resultset was not cached.
#' @param skip_cache            Skip the caching altogether and directly query the database.
#' @param override_cache        If TRUE, the cache will not be loaded and will be overwritten by a new query. For override_cache to take effect, skip_cache should be FALSE.
#' @param conn                  Connection object. If provided, diverts queries to the connection instead of the local Athena instance without caching features.
#' @param render_sql            If TRUE, the SQL will be printed back in the console prior to execution. Default: FALSE
#' @param verbose               If TRUE, prints loading and querying operations messages to the console. Default: FALSE
#' @param sleepTime             Argument for `Sys.sleep()` in between queries to allow for halting function execution, especially in cases where other chariot functions are executing multiple queries in succession and require cancellation.
#' @param cache_resultset       (deprecated) If TRUE, the resultset from the query will first be loaded from the cache. The query will be executed if a cached resultset is not retrieved for this particular query, after which the resultset will be cached. If FALSE, Athena or conn will be directly queried without any caching operations.
#'
#' @return
#' A tibble
#'
#' @seealso
#'  \code{\link[secretary]{typewrite_bold}},\code{\link[secretary]{typewrite}}
#'  \code{\link[stringr]{str_replace}},\code{\link[stringr]{str_remove}}
#'  \code{\link[pg13]{query}},\code{\link[pg13]{cacheQuery}},\code{\link[pg13]{loadCachedQuery}}
#'  \code{\link[tibble]{as_tibble}}
#' @rdname queryAthena
#' @export
#' @importFrom secretary typewrite_bold typewrite
#' @importFrom stringr str_replace_all str_remove_all
#' @importFrom pg13 query cacheQuery loadCachedQuery
#' @importFrom tibble as_tibble


queryAthena <-
        function(sql_statement,
                 conn = NULL,
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 cache_resultset = TRUE,
                 render_sql = FALSE,
                 verbose = FALSE,
                 sleepTime = 1) {

                if (is.null(conn)) {

                        conn_was_missing <- TRUE
                        conn <- connectAthena()
                        on.exit(dcAthena(conn = conn))


                } else {

                        conn_was_missing <- FALSE

                        if (!.hasSlot(conn, name = "jConnection")) {

                                stop('conn object must be a Database Connector JDBC Connection')

                        }

                }

                if (conn_was_missing) {

                        if (skip_cache) {

                                if (verbose) {
                                        secretary::typewrite("Skipping cache")
                                }

                                resultset <- pg13::query(conn = conn,
                                                         sql_statement = sql_statement,
                                                         verbose = verbose,
                                                         render_sql = render_sql)

                        } else {

                                if (override_cache) {

                                        if (verbose) {
                                                secretary::typewrite("Overriding cache")
                                        }

                                        resultset <- pg13::query(conn = conn,
                                                                 sql_statement = sql_statement,
                                                                 verbose = verbose,
                                                                 render_sql = render_sql)

                                        pg13::cacheQuery(resultset,
                                                         sqlQuery = sql_statement,
                                                         db = "athena")


                                } else {

                                        if (verbose) {
                                                secretary::typewrite("Loading Cache")
                                        }


                                        resultset <- tryCatch(pg13::loadCachedQuery(sqlQuery = sql_statement,
                                                                                    db = "athena"),
                                                              error = function(e) NULL)

                                        if (!cache_only) {

                                                if (is.null(resultset)) {


                                                        if (verbose) {
                                                                secretary::typewrite("Cache was NULL, querying Athena")
                                                        }

                                                        Sys.sleep(time = sleepTime)
                                                        resultset <- pg13::query(conn = conn,
                                                                                 sql_statement = sql_statement,
                                                                                 verbose = verbose,
                                                                                 render_sql = render_sql)

                                                        pg13::cacheQuery(resultset,
                                                                         sqlQuery = sql_statement,
                                                                         db = "athena")

                                                }

                                        } else {

                                                if (verbose) {

                                                        secretary::typewrite_bold("Loaded resultset from cache", line_number = 0)

                                                }
                                        }

                                }
                        }

                } else {


                        Sys.sleep(time = sleepTime)
                        resultset <- pg13::query(conn = conn,
                                                 sql_statement = sql_statement,
                                                 verbose = verbose,
                                                 render_sql = render_sql)


                }

                tibble::as_tibble(resultset)

        }


#' Query ancestors for a given concept_id
#' @export

queryAncestors <-
    function(descendant_concept_ids,
             schema,
             min_levels_of_separation = NULL,
             max_levels_of_separation = NULL,
             conn = NULL,
             cache_only = FALSE,
             skip_cache = FALSE,
             override_cache = FALSE,
             render_sql = FALSE,
             verbose = FALSE,
             sleepTime = 1) {

            if (is.null(conn)) {

                    schema <- "public"

            }


            sql_statement <- renderQueryAncestors(descendant_concept_ids = descendant_concept_ids,
                                                  schema = schema,
                                                  min_levels_of_separation = min_levels_of_separation,
                                                  max_levels_of_separation = max_levels_of_separation)

            queryAthena(sql_statement = sql_statement,
                        conn = conn,
                        cache_only = cache_only,
                        skip_cache = skip_cache,
                        override_cache = override_cache,
                        render_sql = render_sql,
                        verbose = verbose,
                        sleepTime = sleepTime)

    }




#' Query based on search terms that does not write to catalogue
#' @param ... vector of codes to collectively feed into the LIKE sql statement
#' @return resultset as a dataframe with all column types as character and trimmed white space
#' @export

queryCode <-
        function(code,
                 schema,
                 caseInsensitive = TRUE,
                 limit = NULL,
                 verbose = FALSE,
                 type = c("like", "exact"),
                 conn = NULL,
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 render_sql = FALSE,
                 sleepTime = 1) {

                if (length(type) != 1) {
                        warning("type is not length 1. Defaulting to 'exact'")
                        type <- "exact"
                }

                if (type == "exact") {

                        sql_statement <-
                                pg13::buildQuery(schema = schema,
                                                 tableName = "concept",
                                                 whereInField = "concept_code",
                                                 whereInVector = code,
                                                 caseInsensitive = caseInsensitive,
                                                 n = limit,
                                                 n_type = "limit")

                } else if (type == "like") {

                        sql_statement <-
                                pg13::buildQueryLike(schema = schema,
                                                     tableName = "concept",
                                                     whereLikeField = "concept_code",
                                                     whereLikeValue = code,
                                                     caseInsensitive = caseInsensitive,
                                                     limit_n = limit)

                } else {
                        stop('type not recognized: ', type)
                }

               queryAthena(sql_statement = sql_statement,
                                         conn = conn,
                                         cache_only = cache_only,
                                         skip_cache = skip_cache,
                                         override_cache = override_cache,
                                         render_sql = render_sql,
                                         verbose = verbose,
                                         sleepTime = sleepTime)
        }




#' Query concept children
#' @import dplyr
#' @export


queryConceptChildren <-
    function(parent_id,
             generations = 1,
             schema,
             conn = NULL,
             cache_only = FALSE,
             skip_cache = FALSE,
             override_cache = FALSE,
             render_sql = FALSE,
             verbose = FALSE,
             sleepTime = 1) {

                sql_statement <- pg13::buildQuery(schema = schema,
                                                  tableName = "concept_parent",
                                                  whereInField = "parent_concept_id",
                                                  whereInVector = parent_id,
                                                  caseInsensitive = FALSE)

                baseline <-
                        queryAthena(sql_statement = sql_statement,
                                    conn = conn,
                                    cache_only = cache_only,
                                    skip_cache = skip_cache,
                                    override_cache = override_cache,
                                    render_sql = render_sql,
                                    verbose = verbose,
                                    sleepTime = sleepTime) %>%
                        dplyr::filter(parent_concept_id != child_concept_id)

                if (nrow(baseline) == 0) {

                        warning('concept "', parent_id, '" has no children')
                        return(baseline)

                }

                output <- list()
                output[[1]] <- baseline


                if (generations > 1) {

                        for (i in 2:generations) {
                                prior <- output[[(i-1)]]

                                if (!is.null(prior)) {

                                        if (nrow(prior) > 0) {

                                                        #Prior child will now be the new parent
                                                        prior <-
                                                                prior %>%
                                                                dplyr::select(new_parent_concept_id = child_concept_id)

                                                        output[[i]] <-
                                                        leftJoinFoChildren(.data = prior,
                                                                           athena_schema = schema,
                                                                           parent_id_column = "new_parent_concept_id",
                                                                           render_sql = render_sql,
                                                                           conn = conn) %>%
                                                                dplyr::select(-any_of("new_parent_concept_id"))

                                        } else {
                                                output[[i]] <- NULL
                                        }

                                } else {
                                        output[[i]] <- NULL
                                }


                        }

                }

                output <-
                        output %>%
                        purrr::keep(~!is.null(.)) %>%
                        purrr::keep(~nrow(.)>0)

                if (length(output) != generations) {

                    message('Maximum possible generations less than "generations" param:', length(output))
                }

                return(output)

    }


#' Query Concept Class Relationships
#' @description This function retrieves the Subject-Predicate-Object triplet from the CONCEPT_RELATIONSHIP table between 2 vocabularies.
#' @return A dataframe with 3 columns: 1) concept_class_id of vocabulary_1 with name as "{vocabulary_id_1}_concept_class_id" unless vocabulary_id_2 is NULL, in which case it will be concept_class_id_1 2) relationship_id from the CONCEPT_RELATIONSHIP table, and 3) concept_class_id of vocabulary_2 with name as "{vocabulary_id_2}_concept_class_id" unless vocabulary_id_2 is NULL, in which case it will be concept_class_id_2.
#' @import SqlRender
#' @param vocabulary_id_1 single vocabulary_id for the first vocabulary (Subject)
#' @param vocabulary_id_2 single vocabulary_id for the second vocabulary (Object). If vocabulary_id_2 is NULL, vocabulary_id_2 is set to vocabulary_id_1.
#' @export


queryConceptClassRelationships <-
    function(vocabulary_id_1,
             vocabulary_id_2 = NULL,
             schema = NULL,
             conn = NULL,
             cache_only = FALSE,
             skip_cache = FALSE,
             override_cache = FALSE,
             render_sql = FALSE,
             verbose = FALSE,
             sleepTime = 1) {

                        if (is.null(schema)) {

                                schema <- "public"

                        }

                        sql_statement <- renderConceptClassRelationships(vocabulary_id_1 = vocabulary_id_1,
                                                                        vocabulary_id_2 = vocabulary_id_2,
                                                                        schema = schema)


                        queryAthena(sql_statement = sql_statement,
                                    conn = conn,
                                    cache_only = cache_only,
                                    skip_cache = skip_cache,
                                    override_cache = override_cache,
                                    render_sql = render_sql,
                                    verbose = verbose,
                                    sleepTime = sleepTime)
    }




#' Lookup a concept id in Athena
#' @import pg13
#' @export

queryConceptId <-
    function(concept_ids,
             schema,
             conn = NULL,
             cache_only = FALSE,
             skip_cache = FALSE,
             override_cache = FALSE,
             render_sql = FALSE,
             verbose = FALSE,
             sleepTime = 1) {


                            # sql <-
                            # pg13::buildQuery(schema = schema,
                            #                  tableName = "concept",
                            #                  whereInField = "concept_id",
                            #                  whereInVector = concept_ids,
                            #                  caseInsensitive = FALSE)

                            sql <-
                                    SqlRender::render("SELECT *
                                                        FROM @schema.concept c
                                                        WHERE c.concept_id IN (@concept_ids)
                                                      ",
                                                        schema = schema,
                                                      concept_ids = concept_ids)


                            queryAthena(sql_statement = sql,
                                        conn = conn,
                                        cache_only = cache_only,
                                        skip_cache = skip_cache,
                                        override_cache = override_cache,
                                        render_sql = render_sql,
                                        verbose = verbose,
                                        sleepTime = sleepTime)

    }



#' Query descendants for a given concept_id
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ancestor_concept_ids PARAM_DESCRIPTION
#' @param schema PARAM_DESCRIPTION
#' @param min_levels_of_separation PARAM_DESCRIPTION, Default: NULL
#' @param max_levels_of_separation PARAM_DESCRIPTION, Default: NULL
#' @param conn PARAM_DESCRIPTION, Default: NULL
#' @param cache_only PARAM_DESCRIPTION, Default: FALSE
#' @param skip_cache PARAM_DESCRIPTION, Default: FALSE
#' @param override_cache PARAM_DESCRIPTION, Default: FALSE
#' @param render_sql PARAM_DESCRIPTION, Default: FALSE
#' @param verbose PARAM_DESCRIPTION, Default: FALSE
#' @param sleepTime PARAM_DESCRIPTION, Default: 1
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname queryDescendants
#' @export
#' @importFrom SqlRender render

queryDescendants <-
    function(ancestor_concept_ids,
             schema,
             min_levels_of_separation = NULL,
             max_levels_of_separation = NULL,
             conn = NULL,
             cache_only = FALSE,
             skip_cache = FALSE,
             override_cache = FALSE,
             render_sql = FALSE,
             verbose = FALSE,
             sleepTime = 1) {
            sql_statement <-
                    SqlRender::render(
                            "
                            SELECT
                                c.concept_id AS ancestor_concept_id,
                                c.concept_name AS ancestor_concept_name,
                                c.domain_id AS ancestor_domain_id,
                                c.vocabulary_id AS ancestor_vocabulary_id,
                                c.concept_class_id AS ancestor_concept_class_id,
                                c.standard_concept AS ancestor_standard_concept,
                                c.concept_code AS ancestor_concept_code,
                                c.valid_start_date AS ancestor_valid_start_date,
                                c.valid_end_date AS ancestor_valid_end_date,
                                c.invalid_reason AS ancestor_invalid_reason,
                                ca.min_levels_of_separation,
                                ca.max_levels_of_separation,
                                c2.concept_id AS descendant_concept_id,
                                c2.concept_name AS descendant_concept_name,
                                c2.domain_id AS descendant_domain_id,
                                c2.vocabulary_id AS descendant_vocabulary_id,
                                c2.concept_class_id AS descendant_concept_class_id,
                                c2.standard_concept AS descendant_standard_concept,
                                c2.concept_code AS descendant_concept_code,
                                c2.valid_start_date AS descendant_valid_start_date,
                                c2.valid_end_date AS descendant_valid_end_date,
                                c2.invalid_reason AS descendant_invalid_reason
                            FROM @schema.concept_ancestor ca
                            INNER JOIN @schema.concept c
                            ON c.concept_id = ca.ancestor_concept_id
                            INNER JOIN @schema.concept c2
                            ON c2.concept_id = ca.descendant_concept_id
                            WHERE
                                ca.ancestor_concept_id IN (@ancestor_concept_ids)
                                AND c.invalid_reason IS NULL
                                AND c2.invalid_reason IS NULL
                            ;",
                            schema = schema,
                            ancestor_concept_ids = ancestor_concept_ids
                    )

            queryAthena(sql_statement = sql_statement,
                        conn = conn,
                        cache_only = cache_only,
                        skip_cache = skip_cache,
                        override_cache = override_cache,
                        render_sql = render_sql,
                        verbose = verbose,
                        sleepTime = sleepTime)

    }



#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ancestor_concept_ids PARAM_DESCRIPTION
#' @param schema PARAM_DESCRIPTION
#' @param min_levels_of_separation PARAM_DESCRIPTION, Default: NULL
#' @param max_levels_of_separation PARAM_DESCRIPTION, Default: NULL
#' @param conn PARAM_DESCRIPTION, Default: NULL
#' @param cache_only PARAM_DESCRIPTION, Default: FALSE
#' @param skip_cache PARAM_DESCRIPTION, Default: FALSE
#' @param override_cache PARAM_DESCRIPTION, Default: FALSE
#' @param render_sql PARAM_DESCRIPTION, Default: FALSE
#' @param verbose PARAM_DESCRIPTION, Default: FALSE
#' @param sleepTime PARAM_DESCRIPTION, Default: 1
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname queryDescendants
#' @export
#' @importFrom SqlRender render

queryRelationships <-
        function(concept_id_1s,
                 schema,
                 relationship_ids = NULL,
                 conn = NULL,
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 render_sql = FALSE,
                 verbose = FALSE,
                 sleepTime = 1) {


                if (is.null(relationship_ids)) {
                sql_statement <-
                        SqlRender::render(
                                "
                            SELECT
                                c.concept_id AS concept_id_1,
                                c.concept_name AS concept_name_1,
                                c.domain_id AS domain_id_1,
                                c.vocabulary_id AS vocabulary_id_1,
                                c.concept_class_id AS concept_class_id_1,
                                c.standard_concept AS standard_concept_1,
                                c.concept_code AS concept_code_1,
                                c.valid_start_date AS valid_start_date_1,
                                c.valid_end_date AS valid_end_date_1,
                                c.invalid_reason AS invalid_reason_1,
                                cr.relationship_id,
                                c2.concept_id AS concept_id_2,
                                c2.concept_name AS concept_name_2,
                                c2.domain_id AS domain_id_2,
                                c2.vocabulary_id AS vocabulary_id_2,
                                c2.concept_class_id AS concept_class_id_2,
                                c2.standard_concept AS concept_2,
                                c2.concept_code AS concept_code_2,
                                c2.valid_start_date AS valid_start_date_2,
                                c2.valid_end_date AS valid_end_date_2,
                                c2.invalid_reason AS invalid_reason_2
                            FROM @schema.concept_relationship cr
                            INNER JOIN @schema.concept c
                            ON c.concept_id = cr.concept_id_1
                            INNER JOIN @schema.concept c2
                            ON c2.concept_id = cr.concept_id_2
                            WHERE
                                cr.concept_id_1 IN (@concept_id_1s)
                                AND c.invalid_reason IS NULL
                                AND cr.invalid_reason IS NULL
                                AND c2.invalid_reason IS NULL
                            ;",
                                schema = schema,
                                concept_id_1s = concept_id_1s
                        )
                } else {

                        relationship_ids <- paste0("'", tolower(relationship_ids), "'")

                        sql_statement <-
                                SqlRender::render(
                                        "
                            SELECT
                                c.concept_id AS concept_id_1,
                                c.concept_name AS concept_name_1,
                                c.domain_id AS domain_id_1,
                                c.vocabulary_id AS vocabulary_id_1,
                                c.concept_class_id AS concept_class_id_1,
                                c.standard_concept AS standard_concept_1,
                                c.concept_code AS concept_code_1,
                                c.valid_start_date AS valid_start_date_1,
                                c.valid_end_date AS valid_end_date_1,
                                c.invalid_reason AS invalid_reason_1,
                                cr.relationship_id,
                                c2.concept_id AS concept_id_2,
                                c2.concept_name AS concept_name_2,
                                c2.domain_id AS domain_id_2,
                                c2.vocabulary_id AS vocabulary_id_2,
                                c2.concept_class_id AS concept_class_id_2,
                                c2.standard_concept AS standard_concept_2,
                                c2.concept_code AS concept_code_2,
                                c2.valid_start_date AS valid_start_date_2,
                                c2.valid_end_date AS valid_end_date_2,
                                c2.invalid_reason AS invalid_reason_2
                            FROM @schema.concept_relationship cr
                            INNER JOIN @schema.concept c
                            ON c.concept_id = cr.concept_id_1
                            INNER JOIN @schema.concept c2
                            ON c2.concept_id = cr.concept_id_2
                            WHERE
                                cr.concept_id_1 IN (@concept_id_1s)
                                AND LOWER(cr.relationship_id) IN (@relationship_ids)
                                AND c.invalid_reason IS NULL
                                AND cr.invalid_reason IS NULL
                                AND c2.invalid_reason IS NULL
                            ;",
                                        schema = schema,
                                        concept_id_1s = concept_id_1s,
                                        relationship_ids = relationship_ids
                                )

                }

                queryAthena(sql_statement = sql_statement,
                            conn = conn,
                            cache_only = cache_only,
                            skip_cache = skip_cache,
                            override_cache = override_cache,
                            render_sql = render_sql,
                            verbose = verbose,
                            sleepTime = sleepTime)

        }


#' Combine Parents and Children Relationships
#' @import purrr
#' @export

queryFamilyTree <-
	function(concept_id,
	         schema,
	         parental_generations = 1,
	         child_generations = 1,
	         conn = NULL,
	         cache_only = FALSE,
	         skip_cache = FALSE,
	         override_cache = FALSE,
	         render_sql = FALSE,
	         verbose = FALSE,
	         sleepTime = 1) {


	        if (parental_generations != 0) {

        	        parents <- queryConceptParent(child_id = concept_id,
        	                                      schema = schema,
        	                                      generations = parental_generations,
        	                                      verbose = verbose,
        	                                      cache_resultset = cache_resultset,
        	                                      override_cache = override_cache,
        	                                      conn = conn,
        	                                      render_sql = render_sql,
        	                                      sleepTime = sleepTime,
        	                                      ...)
	        } else {
	                parents <- NULL
	        }

	        if (child_generations != 0) {

        	        children <- queryConceptChildren(parent_id = concept_id,
        	                                         schema = schema,
        	                                         generations = child_generations,
        	                                         verbose = verbose,
        	                                         cache_resultset = cache_resultset,
        	                                         override_cache = override_cache,
        	                                         conn = conn,
        	                                         render_sql = render_sql,
        	                                         sleepTime = sleepTime,
        	                                         ...)

	        } else {

	                children <- NULL


	        }



        	        final <- c(parents,
        	                   children) %>%
        	                purrr::keep(~is.data.frame(.))

        	        return(final)

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

                # For inputs that are actually regimens, a new set of components is derived.
                component_concept_ids <-
                        normalizeToHemOncComponents(hemonc_concept_ids = component_concept_ids,
                                                    schema = schema)

                # Get input component count to filter HemOnc Regimens based on their own component_counts
                input_component_count <- length(component_concept_ids)

                # If any of the concept_ids are regimens, to get their antineoplastic components
                input_concept <-
                        queryConceptId(component_concept_ids,
                                       schema = schema)

                qa <- input_concept %>%
                        rubix::filter_for(filter_col = concept_class_id,
                                          inclusion_vector = c("Regimen",
                                                               "Component"),
                                          invert = TRUE)

                if (nrow(qa)) {
                        qaHemOncCompToReg <<- qa
                        stop('input concept ids are not Regimen or Components. See qaHemOncCompToReg for more details.')
                }


                # Query Athena DB for all Regimens associated with the inputted Component Concept Ids
                sql_statement <-
                renderHemOncCompToReg(component_concept_ids = component_concept_ids,
                                      schema = schema)

                Regimens <-
                        queryAthena(sql_statement = sql_statement,
                                    conn = conn,
                                    cache_only = cache_only,
                                    skip_cache = skip_cache,
                                    override_cache = override_cache,
                                    render_sql = render_sql,
                                    verbose = verbose,
                                    sleepTime = sleepTime)

                # Query again to get all of the "Has antineoplastic" relationships to HemOnc Components these Regimens have
                HasAntineoplastics <- queryHemOncRegToAntineo(regimen_concept_ids = Regimens$regimen_concept_id,
                                                              schema = schema)



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

                #If only 1 or less rows, the function is complete. Otherwise, the outputs need to be filtered another time since now we have all the Regimens that have the exact component count match as the input and have at least 1 of the input components, but does not necessarily have all the components
#
                if (nrow(HasAntineoplastics2) <= 1) {
                        return(HasAntineoplastics2)
                } else {
                        HasAntineoplastics3 <-
                                HasAntineoplastics %>%
                                dplyr::select(regimen_concept_id,
                                              has_antineoplastic_concept_id) %>%
                                dplyr::group_by(regimen_concept_id) %>%
                                dplyr::summarise_at(vars(has_antineoplastic_concept_id),
                                                    list(has_all_components = ~all(. %in% component_concept_ids),
                                                         component_count = ~length(unique(.)))) %>%
                                dplyr::filter(has_all_components == TRUE,
                                              component_count == input_component_count) %>%
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

                sql_statement <-
                        renderHemOncRegToAntineoplastics(regimen_concept_ids = regimen_concept_ids,
                                                         schema = schema)

                queryAthena(sql_statement = sql_statement,
                            conn = conn,
                            cache_only = cache_only,
                            skip_cache = skip_cache,
                            override_cache = override_cache,
                            render_sql = render_sql,
                            verbose = verbose,
                            sleepTime = sleepTime)
        }




#' Perform a Global Query on a Single Phrase
#' @export

queryPhrase <-
        function(schema,
                 phrase,
                 split,
                 caseInsensitive = TRUE,
                 concept_table_search = c("exact", "string", "like"),
                 concept_synonym_table_search = c("exact", "string", "like"),
                 conn = NULL,
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 render_sql = FALSE,
                 verbose = FALSE,
                 sleepTime = 1) {

                if (any(!(concept_table_search %in% c("exact", "string", "like")))) {
                        stop("'concept_table_search' can only be one of c('exact', 'string', 'like')")
                }

                if (any(!(concept_synonym_table_search %in% c("exact", "string", "like")))) {
                        stop("'concept_synonym_table_search' can only be one of c('exact', 'string', 'like')")
                }

                concept_resultsets <- list()

                while (length(concept_table_search) > 0) {

                        search <- concept_table_search[1]

                        if (search == "exact") {

                                concept_resultsets[[1+length(concept_resultsets)]] <-
                                        queryPhraseExact(schema = schema,
                                                         phrase = phrase,
                                                         caseInsensitive = caseInsensitive,
                                                         conn = conn,
                                                         render_sql = render_sql,
                                                         cache_resultset = cache_resultset,
                                                         override_cache = override_cache,
                                                         verbose = verbose)

                        } else if (search == "like") {

                                concept_resultsets[[1+length(concept_resultsets)]] <-
                                        queryPhraseLike(schema = schema,
                                                         phrase = phrase,
                                                         caseInsensitive = caseInsensitive,
                                                         conn = conn,
                                                         render_sql = render_sql,
                                                         cache_resultset = cache_resultset,
                                                         override_cache = override_cache,
                                                         verbose = verbose)

                        } else if (search == "string") {

                                concept_resultsets[[1+length(concept_resultsets)]] <-
                                        queryPhraseString(schema = schema,
                                                        phrase = phrase,
                                                        split = split,
                                                        caseInsensitive = caseInsensitive,
                                                        conn = conn,
                                                        render_sql = render_sql,
                                                        cache_resultset = cache_resultset,
                                                        override_cache = override_cache,
                                                        verbose = verbose)


                        }

                        names(concept_resultsets)[length(concept_resultsets)] <- search
                        concept_table_search <- concept_table_search[-1]
                        Sys.sleep(.2)
                }

                while (length(concept_synonym_table_search) > 0) {

                        search <- concept_synonym_table_search[1]

                        if (search == "exact") {

                                concept_resultsets[[1+length(concept_resultsets)]] <-
                                        queryPhraseExactSynonym(schema = schema,
                                                         phrase = phrase,
                                                         caseInsensitive = caseInsensitive,
                                                         conn = conn,
                                                         render_sql = render_sql,
                                                         cache_resultset = cache_resultset,
                                                         override_cache = override_cache,
                                                         verbose = verbose)

                        } else if (search == "like") {

                                concept_resultsets[[1+length(concept_resultsets)]] <-
                                        queryPhraseLikeSynonym(schema = schema,
                                                        phrase = phrase,
                                                        caseInsensitive = caseInsensitive,
                                                        conn = conn,
                                                        render_sql = render_sql,
                                                        cache_resultset = cache_resultset,
                                                        override_cache = override_cache,
                                                        verbose = verbose)

                        } else if (search == "string") {

                                concept_resultsets[[1+length(concept_resultsets)]] <-
                                        queryPhraseStringSynonym(schema = schema,
                                                          phrase = phrase,
                                                          split = split,
                                                          caseInsensitive = caseInsensitive,
                                                          conn = conn,
                                                          render_sql = render_sql,
                                                          cache_resultset = cache_resultset,
                                                          override_cache = override_cache,
                                                          verbose = verbose)


                        }

                        names(concept_resultsets)[length(concept_resultsets)] <- paste0("synonym_", search)
                        concept_synonym_table_search <- concept_synonym_table_search[-1]
                        Sys.sleep(.2)
                }


                return(concept_resultsets)

        }




#' Query Exact Phrase
#' @import pg13
#' @export

queryPhraseExact <-
        function(schema,
                 phrase,
                 case_insensitive,
                 conn = NULL,
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 render_sql = FALSE,
                 verbose = FALSE,
                 sleepTime = 1) {


                if (case_insensitive) {

                        sql_statement <-
                                SqlRender::render(
                                        "
                                SELECT *
                                FROM @schema.@table c
                                WHERE LOWER(c.concept_name) = LOWER('@phrase')
                                ",
                                        schema = schema,
                                        phrase = phrase,
                                        table = "concept"
                                )


                } else {

                        sql_statement <-
                                SqlRender::render(
                                        "
                                SELECT *
                                FROM @schema.@table c
                                WHERE c.concept_name = '@phrase'
                                ",
                                        schema = schema,
                                        phrase = phrase,
                                        table = "concept"
                                )

                }


                queryAthena(sql_statement = sql_statement,
                              conn = conn,
                              cache_only = cache_only,
                              skip_cache = skip_cache,
                              override_cache = override_cache,
                              render_sql = render_sql,
                              verbose = verbose,
                              sleepTime = sleepTime)
        }




#' Query Concept Synonym Table
#' @import pg13
#' @import secretary
#' @import dplyr
#' @import rubix
#' @export


queryPhraseExactSynonym <-
        function(schema,
                 case_insensitive = TRUE,
                 phrase,
                 conn = NULL,
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 render_sql = FALSE,
                 verbose = FALSE,
                 sleepTime = 1) {

                if (case_insensitive) {

                        sql_statement <-
                                SqlRender::render(
                                        "
                                SELECT *
                                FROM @schema.@table c
                                WHERE LOWER(c.concept_synonym_name) = LOWER('@phrase')
                                ",
                                        schema = schema,
                                        phrase = phrase,
                                        table = "concept_synonym"
                                )


                } else {

                        sql_statement <-
                                SqlRender::render(
                                        "
                                SELECT *
                                FROM @schema.@table c
                                WHERE c.concept_synonym_name = '@phrase'
                                ",
                                        schema = schema,
                                        phrase = phrase,
                                        table = "concept_synonym"
                                )

                }


                queryAthena(sql_statement = sql_statement,
                            conn = conn,
                            cache_only = cache_only,
                            skip_cache = skip_cache,
                            override_cache = override_cache,
                            render_sql = render_sql,
                            verbose = verbose,
                            sleepTime = sleepTime)


        }




#' Query Like Phrase
#' @import pg13
#' @export

queryPhraseLike <-
        function(schema,
                 phrase,
                 caseInsensitive,
                 conn = NULL,
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 render_sql = FALSE,
                 verbose = FALSE,
                 sleepTime = 1) {

                sql_statement <-
                        pg13::buildQueryLike(tableName = "concept",
                                             schema = schema,
                                             whereLikeField = "concept_name",
                                             whereLikeValue = phrase,
                                             caseInsensitive = caseInsensitive)


                queryAthena(sql_statement = sql_statement,
                            conn = conn,
                            cache_only = cache_only,
                            skip_cache = skip_cache,
                            override_cache = override_cache,
                            render_sql = render_sql,
                            verbose = verbose,
                            sleepTime = sleepTime)
        }




#' Query Concept Synonym Table for Concepts Like Phrase
#' @import pg13
#' @import secretary
#' @import dplyr
#' @import rubix
#' @export


queryPhraseLikeSynonym <-
        function(schema,
                 caseInsensitive = TRUE,
                 phrase,
                 conn = NULL,
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 render_sql = FALSE,
                 verbose = FALSE,
                 sleepTime = 1) {

                sql_statement <-
                        renderQueryPhraseLikeSynonym(schema = schema,
                                                     phrase = phrase,
                                                     caseInsensitive = caseInsensitive)


                queryAthena(sql_statement = sql_statement,
                            conn = conn,
                            cache_only = cache_only,
                            skip_cache = skip_cache,
                            override_cache = override_cache,
                            render_sql = render_sql,
                            verbose = verbose,
                            sleepTime = sleepTime)

        }




#' Query Concept Table for a Phrase Split into Strings
#' @import pg13
#' @import secretary
#' @export


queryPhraseString <-
        function(schema,
                 caseInsensitive = TRUE,
                 phrase,
                 split = " ",
                 conn = NULL,
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 render_sql = FALSE,
                 verbose = FALSE,
                 sleepTime = 1) {

                sql_statement <-
                        pg13::buildQueryString(schema = schema,
                                               tableName = "concept",
                                               whereLikeField = "concept_name",
                                               string=phrase,
                                               split=split,
                                               caseInsensitive = caseInsensitive)

                queryAthena(sql_statement = sql_statement,
                            conn = conn,
                            cache_only = cache_only,
                            skip_cache = skip_cache,
                            override_cache = override_cache,
                            render_sql = render_sql,
                            verbose = verbose,
                            sleepTime = sleepTime)

        }




#' Query Concept Synonym Table for a Phrase Split into Strings
#' @import pg13
#' @import secretary
#' @import dplyr
#' @import rubix
#' @export


queryPhraseStringSynonym <-
        function(schema,
                 caseInsensitive = TRUE,
                 phrase,
                 split = " ",
                 conn = NULL,
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 render_sql = FALSE,
                 verbose = FALSE,
                 sleepTime = 1) {

                sql_statement <-
                        pg13::buildQueryString(schema = schema,
                                               tableName = "concept_synonym",
                                               whereLikeField = "concept_synonym_name",
                                               string=phrase,
                                               split=split,
                                               caseInsensitive = caseInsensitive)

                output1 <-
                        queryAthena(sql_statement = sql_statement,
                                    conn = conn,
                                    cache_only = cache_only,
                                    skip_cache = skip_cache,
                                    override_cache = override_cache,
                                    render_sql = render_sql,
                                    verbose = verbose,
                                    sleepTime = sleepTime) %>%
                        dplyr::rename(concept_synonym_id = concept_id)



                leftJoinConcept(output1,
                                column = "concept_synonym_id",
                                athena_schema = schema,
                                conn = conn,
                                render_sql = render_sql) %>%
                        dplyr::filter(concept_name != concept_synonym_name) %>%
                        dplyr::select(-concept_synonym_id) %>%
                        rubix::group_by_unique_aggregate(concept_id,
                                                         concept_name,
                                                         domain_id,
                                                         vocabulary_id,
                                                         concept_class_id,
                                                         standard_concept,
                                                         concept_code,
                                                         valid_start_date,
                                                         valid_end_date,
                                                         invalid_reason,
                                                         agg.col = concept_synonym_name)

        }




#' Query the synonyms associated with an id
#' @inheritParams write_sql_to_get_synonyms
#' @importFrom dplyr select
#' @export

querySynonyms <-
    function(concept_id,
             schema = NULL,
             language_concept_id = 4180186,
             conn = NULL,
             cache_only = FALSE,
             skip_cache = FALSE,
             override_cache = FALSE,
             render_sql = FALSE,
             verbose = FALSE,
             sleepTime = 1) {

                sql_statement <- renderSynonyms(concept_id = concept_id,
                                      schema = schema,
                                      language_concept_id = language_concept_id)

                queryAthena(sql_statement = sql_statement,
                            conn = conn,
                            cache_only = cache_only,
                            skip_cache = skip_cache,
                            override_cache = override_cache,
                            render_sql = render_sql,
                            verbose = verbose,
                            sleepTime = sleepTime) %>%
                               dplyr::select(concept_synonym_name) %>%
                               unlist()
    }




#' Vocabulary Relationships
#' @description This function retrieves all the relationships of a given vocabulary, identified by the OMOP vocabulary_id.
#' @return dataframe derived from the concept_relationship table where concept_id_1 are all the concepts belonging to the vocabulary_id argument.
#' @import SqlRender
#' @export


queryVocabularyRelationships <-
    function(vocabulary_id,
             verbose = FALSE,
             conn = NULL,
             cache_only = FALSE,
             skip_cache = FALSE,
             override_cache = FALSE,
             render_sql = FALSE,
             sleepTime = 1) {

                        base <- system.file(package = "chariot")
                        path <- paste0(base, "/sql/vocabularyRelationship.sql")

                        sql_statement <-
                                SqlRender::render(SqlRender::readSql(sourceFile = path),
                                                  vocabulary_id = vocabulary_id)

                        queryAthena(sql_statement = sql_statement,
                                    conn = conn,
                                    cache_only = cache_only,
                                    skip_cache = skip_cache,
                                    override_cache = override_cache,
                                    render_sql = render_sql,
                                    verbose = verbose,
                                    sleepTime = sleepTime)
    }


