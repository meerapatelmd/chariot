#' Join a dataframe object with Athena's concept table
#' @description This function has better performance than the WHERE IN statement for larger searches. A table in the format of "v{unpunctuated timestamp}" is written to the local Athena. A join is performed with a concept table. The table is subsequently dropped. If a dataframe column is not provided as a the column to join the concept table on, the 1st column will be used by default.
#' @param dataframe dataframe to join
#' @param column string of the column name to join on. If NULL, the 1st column is used.
#' @param athena_column name of column to join dataframe on. Defaults to concept ID.
#' @export

join <-
    function(.data,
             joinType,
             column = NULL,
             athena_schema,
             athena_table,
             athena_column,
             where_athena_col = NULL,
             where_athena_col_in = NULL,
             verbose = FALSE,
             conn = NULL,
             render_sql = FALSE,
             sleepTime = 1) {


        table_name <- paste0("v", stampede::stamp_this(without_punct = TRUE))

        if (is.null(column)) {
            column <- colnames(.data)[1]
        }


        if (is.null(conn)) {

            conn <- connectAthena()
            pg13::writeTable(conn = conn,
                             schema = athena_schema,
                             tableName = table_name,
                             .data = .data)
            dcAthena(conn = conn)


            if (!is.null(where_athena_col)) {

                where_athena_col <- paste0(athena_schema,".",
                                           athena_table, ".",
                                           where_athena_col)
            }

            sql_statement <-
                pg13::buildJoinQuery(schema = "public",
                                     tableName = table_name,
                                     column = column,
                                     joinType = joinType,
                                     caseInsensitive = FALSE,
                                     joinOnSchema = athena_schema,
                                     joinOnTableName = athena_table,
                                     joinOnColumn = athena_column,
                                     whereInField = where_athena_col,
                                     whereInVector = where_athena_col_in)



            resultset <- queryAthena(sql_statement = sql_statement,
                                     verbose = verbose,
                                     cache_resultset = FALSE,
                                     render_sql = render_sql,
                                     sleepTime = sleepTime)



            conn <- connectAthena()
            dropJoinTables(conn = conn,
                           schema = athena_schema)
            dcAthena(conn = conn)


        } else {

            pg13::writeTable(conn = conn,
                             schema = athena_schema,
                             tableName = table_name,
                             .data = .data)


            if (!is.null(where_athena_col)) {

                where_athena_col <- paste0(athena_schema,".",
                                           athena_table, ".",
                                           where_athena_col)
            }

            sql_statement <-
                pg13::buildJoinQuery(schema = "public",
                                     tableName = table_name,
                                     column = column,
                                     joinType = joinType,
                                     caseInsensitive = FALSE,
                                     joinOnSchema = athena_schema,
                                     joinOnTableName = athena_table,
                                     joinOnColumn = athena_column,
                                     whereInField = where_athena_col,
                                     whereInVector = where_athena_col_in)



            resultset <- queryAthena(sql_statement = sql_statement,
                                     verbose = verbose,
                                     skip_cache = TRUE,
                                     conn = conn,
                                     render_sql = render_sql,
                                     sleepTime = sleepTime)

            dropJoinTables(conn = conn,
                           schema = athena_schema)
        }

        return(resultset)
    }








#' Drop Join Tables
#' @description Drops all tables in the format ("V{timestamp}")
#' @import DatabaseConnector
#' @export

dropJoinTables <-
    function(conn,
             schema = NULL) {

            joinTables  <-  lsJoinTables(conn = conn,
                                         schema = schema)

            for (joinTable in joinTables) {

                pg13::dropTable(conn = conn,
                                schema = schema,
                                tableName = joinTable)

            }

    }




#' INNER JOIN an OMOP Vocabulary Table
#' @description This function executes the join() function with joinType == "INNER".
#' @export

innerJoin <-
    function(.data,
             column = NULL,
             athena_schema,
             athena_table,
             athena_column,
             where_athena_col = NULL,
             where_athena_col_in = NULL,
             render_sql = TRUE,
             conn = NULL) {


                    join(.data = .data,
                         joinType = "INNER",
                         column = column,
                         athena_schema = athena_schema,
                         athena_table = athena_table,
                         athena_column = athena_column,
                         where_athena_col = where_athena_col_in,
                         where_athena_col_in = where_athena_col_in,
                         render_sql = render_sql,
                         conn = conn)

    }




#' LEFT JOIN an OMOP Vocabulary Table
#' @description
#' This function executes the join() function with joinType == "LEFT".
#' @export

leftJoin <-
    function(.data,
             column = NULL,
             athena_schema,
             athena_table,
             athena_column,
             where_athena_col = NULL,
             where_athena_col_in = NULL,
             verbose = FALSE,
             conn = NULL,
             render_sql = FALSE,
             sleepTime = 1) {



                    join(.data = .data,
                         joinType = "LEFT",
                         column = column,
                         athena_schema = athena_schema,
                         athena_table = athena_table,
                         athena_column = athena_column,
                         where_athena_col = where_athena_col,
                         where_athena_col_in = where_athena_col_in,
                         verbose = verbose,
                         conn = conn,
                         render_sql = render_sql,
                         sleepTime = sleepTime)

    }




#' @title Join a dataframe object with the Concept Table
#' @description
#' This function has better performance than the WHERE IN statement for larger searches. A table in the format of "v{unpunctuated timestamp}" is written to the local Athena. A join is performed with a concept table. The table is subsequently dropped. If a dataframe column is not provided as a the column to join the concept table on, the 1st column will be used by default.
#'
#' @param .data                 A data frame
#' @param column                Data frame column that the join will be performed on. If NULL, defaults to the column in position 1 of the data frame.
#' @param athena_schema         Schema of the OMOP Concept Table
#' @param concept_column        Column in the concept
#' @param verbose               If TRUE, prints whether the cache is being loaded or being actively queried in the Postgres database, Default: FALSE
#' @param conn                  Connection object if another database is used. Default: NULL
#' @param render_sql            If TRUE, will print the SQL to the console before executing. Default: FALSE
#' @param sleepTime             Argument in seconds passed to the `Sys.sleep()` function at the end of query, Default: 1
#' @param ...                   Additional arguments passed to the `queryAthena()` function.
#'
#' @return
#' A data frame
#'
#' @seealso
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{filter}},\code{\link[dplyr]{mutate-joins}}
#'  \code{\link[rubix]{group_by_unique_aggregate}}
#' @rdname leftJoinConcept
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr select filter left_join
#' @importFrom rubix group_by_unique_aggregate


leftJoinConcept <-
    function(.data,
             column = NULL,
             athena_schema = "public",
             concept_column = "concept_id",
             synonyms = FALSE,
             verbose = FALSE,
             conn = NULL,
             render_sql = FALSE,
             sleepTime = 1) {


                            if (is.null(column)) {
                                column <- colnames(.data)[1]
                            }

                            if (column == concept_column) {
                                stop("'column' parameter cannot be equal to 'concept_column'")
                            }

                            output <-
                                leftJoin(.data = .data,
                                         column = column,
                                         athena_schema = athena_schema,
                                         athena_table = "concept",
                                         athena_column = concept_column,
                                         verbose = verbose,
                                         conn = conn,
                                         render_sql = render_sql,
                                         sleepTime = sleepTime)


                            if (synonyms) {

                                    output_s <-
                                            leftJoinSynonymId(.data = output %>%
                                                                        dplyr::select(-concept_id),
                                                              column = column,
                                                              athena_schema = athena_schema,
                                                              verbose = verbose,
                                                              conn = conn,
                                                              render_sql = render_sql,
                                                              sleepTime = sleepTime) %>%
                                            dplyr::filter(concept_name != concept_synonym_name) %>%
                                            rubix::group_by_unique_aggregate(concept_id,
                                                                             agg.col = concept_synonym_name,
                                                                             collapse = "|")


                                    output <-
                                            output %>%
                                            dplyr::left_join(output_s)

                            }



                    return(output)

    }


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param .data PARAM_DESCRIPTION
#' @param column PARAM_DESCRIPTION, Default: NULL
#' @param athena_schema PARAM_DESCRIPTION, Default: 'public'
#' @param concept_synonym_column PARAM_DESCRIPTION, Default: 'concept_synonm_name'
#' @param verbose PARAM_DESCRIPTION, Default: FALSE
#' @param conn PARAM_DESCRIPTION, Default: NULL
#' @param render_sql PARAM_DESCRIPTION, Default: FALSE
#' @param sleepTime PARAM_DESCRIPTION, Default: 1
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @seealso
#'  \code{\link[stampede]{stamp_this}}
#'  \code{\link[pg13]{writeTable}}
#'  \code{\link[SqlRender]{render}}
#' @rdname leftJoinSynonymNames
#' @export
#' @importFrom stampede stamp_this
#' @importFrom pg13 writeTable
#' @importFrom SqlRender render

leftJoinSynonymNames <-
    function(.data,
             column = NULL,
             write_schema = "public",
             verbose = FALSE,
             render_sql = FALSE,
             sleepTime = 1,
             vocabulary_id,
             domain_id,
             concept_class_id,
             standard_concept,
             invalid_reason,
             conn = NULL,
             omop_vocabulary_schema) {


        if (!is.null(conn)) {

            if (missing(omop_vocabulary_schema)) {

                stop("'omop_vocabulary_schema required to run query")

            }

        } else {

            omop_vocabulary_schema <- "public"

        }


        where_clauses <- vector()
        where_clauses_fields <- vector()
        if (!missing(vocabulary_id)) {

                where_clauses_fields <-
                    c(where_clauses_fields,
                      "vocabulary_id")

                vocabulary_id <- paste0("'", vocabulary_id, "'")
                where_clauses <-
                    c(where_clauses,
                      SqlRender::render("@omop_vocabulary_schema.concept.vocabulary_id IN (@vocabulary_id)\n", vocabulary_id = vocabulary_id))
        }

        if (!missing(domain_id)) {

            where_clauses_fields <-
                c(where_clauses_fields,
                  "domain_id")

            domain_id <- paste0("'", domain_id, "'")
            where_clauses <-
                c(where_clauses,
                  SqlRender::render("@omop_vocabulary_schema.concept.domain_id IN (@domain_id)\n", domain_id = domain_id))
        }

        if (!missing(concept_class_id)) {

            where_clauses_fields <-
                c(where_clauses_fields,
                  "concept_class_id")

            concept_class_id <- paste0("'", concept_class_id, "'")
            where_clauses <-
                c(where_clauses,
                  SqlRender::render("@omop_vocabulary_schema.concept.concept_class_id IN (@concept_class_id)\n", concept_class_id = concept_class_id))

        }

        if (!missing(standard_concept)) {

            where_clauses_fields <-
                c(where_clauses_fields,
                  "standard_concept")

            if (any("NULL" %in% standard_concept)) {

                    part_a <- "@omop_vocabulary_schema.concept.standard_concept IS NULL"

            } else {
                    part_a <- vector()
            }

            standard_concept <- standard_concept[!(standard_concept %in% "NULL")]

            if (length(standard_concept)) {

                    standard_concept <- paste0("'", standard_concept, "'")

                    part_b <- SqlRender::render("@omop_vocabulary_schema.concept.standard_concept IN (@standard_concept)", standard_concept = standard_concept)

            } else {

                    part_b <- vector()
            }

            clause_with_null <- c(part_a, part_b) %>% paste(collapse = " OR ")

            where_clauses <-
                c(where_clauses,
                  clause_with_null)

        }


        if (!missing(invalid_reason)) {

            where_clauses_fields <-
                c(where_clauses_fields,
                  "invalid_reason")

            if (any("NULL" %in% invalid_reason)) {

                part_a <- "@omop_vocabulary_schema.concept.invalid_reason IS NULL"

            } else {
                part_a <- vector()
            }

            invalid_reason <- invalid_reason[!(invalid_reason %in% "NULL")]

            if (length(invalid_reason)) {

                invalid_reason <- paste0("'", invalid_reason, "'")

                part_b <- SqlRender::render("@omop_vocabulary_schema.concept.invalid_reason IN (@invalid_reason)", invalid_reason = invalid_reason)

            } else {

                part_b <- vector()
            }

            clause_with_null <- c(part_a, part_b) %>% paste(collapse = " OR ")


            where_clauses <-
                c(where_clauses,
                  clause_with_null)

        }

        if (length(where_clauses)) {

            where_clauses <- paste(where_clauses, collapse = " AND ")

        }


        table_name <- paste0("v", stampede::stamp_this(without_punct = TRUE))

        if (is.null(column)) {
            column <- colnames(.data)[1]
        }


        if (is.null(conn)) {

                        conn <- connectAthena()
                        pg13::writeTable(conn = conn,
                                         schema = write_schema,
                                         tableName = table_name,
                                         .data = .data)
                        dcAthena(conn = conn)


                    if (length(where_clauses) == 0) {

                        sql_statement <-
                           SqlRender::render("SELECT *
                                                FROM @write_schema.@table_name a
                                                LEFT JOIN @omop_vocabulary_schema.concept_synonym cs
                                                ON LOWER(cs.concept_synonym_name) = LOWER(a.@column);",
                                             omop_vocabulary_schema = omop_vocabulary_schema,
                                             table_name = table_name,
                                             column = column,
                                             write_schema = write_schema
                                             )
                    } else {

                        sql_statement <-
                            SqlRender::render(paste0(
                                                "
                                                WITH omop_concepts AS (
                                                            SELECT @omop_vocabulary_schema.concept_synonym.*
                                                            FROM @omop_vocabulary_schema.concept
                                                            INNER JOIN @omop_vocabulary_schema.concept_synonym
                                                            ON @omop_vocabulary_schema.concept_synonym.concept_id = c.concept_id
                                                            WHERE ", where_clauses,
                                                ")

                                                SELECT a.*, omop.*
                                                FROM @write_schema.@table_name a
                                                LEFT JOIN omop_concepts omop
                                                ON LOWER(omop_concepts.concept_synonym_name) = LOWER(a.@column)"),
                                              omop_vocabulary_schema = omop_vocabulary_schema,
                                              table_name = table_name,
                                              column = column,
                                              write_schema = write_schema
                            )

                    }

                        resultset <- queryAthena(sql_statement = sql_statement,
                                                 verbose = verbose,
                                                 skip_cache = TRUE,
                                                 render_sql = render_sql,
                                                 sleepTime = sleepTime)



                        conn <- connectAthena()
                        dropJoinTables(conn = conn,
                                       schema = write_schema)
                        dcAthena(conn = conn)

                        resultset


        } else {

            pg13::writeTable(conn = conn,
                             schema = write_schema,
                             tableName = table_name,
                             .data = .data)

            if (length(where_clauses) == 0) {

                sql_statement <-
                    SqlRender::render("SELECT *
                                                FROM @write_schema.@table_name a
                                                LEFT JOIN @omop_vocabulary_schema.concept_synonym cs
                                                ON LOWER(cs.concept_synonym_name) = LOWER(a.@column);",
                                      omop_vocabulary_schema = omop_vocabulary_schema,
                                      table_name = table_name,
                                      column = column,
                                      write_schema = write_schema
                    )
            } else {

                sql_statement <-
                    SqlRender::render(paste0(
                    "
                    WITH omop_concepts AS (
                        SELECT @omop_vocabulary_schema.concept_synonym.*
                            FROM @omop_vocabulary_schema.concept
                        INNER JOIN @omop_vocabulary_schema.concept_synonym
                        ON @omop_vocabulary_schema.concept_synonym.concept_id = c.concept_id
                        WHERE ", where_clauses,
                                                ")

                        SELECT a.*, omop.*
                            FROM @write_schema.@table_name a
                        LEFT JOIN omop_concepts omop
                        ON LOWER(omop_concepts.concept_synonym_name) = LOWER(a.@column)"),
                        omop_vocabulary_schema = omop_vocabulary_schema,
                        table_name = table_name,
                        column = column,
                        write_schema = write_schema
                    )

            }

            resultset <- queryAthena(conn = conn,
                                    sql_statement = sql_statement,
                                     verbose = verbose,
                                     skip_cache = TRUE,
                                     render_sql = render_sql,
                                     sleepTime = sleepTime)

            dropJoinTables(conn = conn,
                           schema = write_schema)

            resultset


        }
    }





#' @title Left Join a data frame to the Concept Ancestor Table
#' @param .data PARAM_DESCRIPTION
#' @param athena_schema PARAM_DESCRIPTION, Default: 'public'
#' @param descendant_id_column PARAM_DESCRIPTION, Default: NULL
#' @param whereLevelIn PARAM_DESCRIPTION, Default: NULL
#' @param whereLevelType PARAM_DESCRIPTION, Default: NULL
#' @param verbose PARAM_DESCRIPTION, Default: FALSE
#' @param conn PARAM_DESCRIPTION, Default: NULL
#' @param render_sql PARAM_DESCRIPTION, Default: FALSE
#' @param sleepTime PARAM_DESCRIPTION, Default: 1
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @seealso
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{distinct}},\code{\link[dplyr]{mutate-joins}}
#'  \code{\link[rubix]{rename_all_with_prefix}}
#' @rdname leftJoinForAncestors
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr select distinct left_join
#' @importFrom rubix rename_all_with_prefix

leftJoinForAncestors <-
        function(.data,
                 athena_schema = "public",
                 descendant_id_column = NULL,
                 whereLevelIn = NULL,
                 whereLevelType = NULL,
                 verbose = FALSE,
                 conn = NULL,
                 render_sql = FALSE,
                 sleepTime = 1,
                 ...) {

                        if (!is.null(whereLevelIn) && length(whereLevelType) != 1) {


                                warning("No 'whereLevelType'. Defaulting to 'max'")
                                whereLevelType <- "max"

                        }

                        if (!is.null(whereLevelIn)) {

                                if (whereLevelType == "max") {
                                        whereAthenaField <- "max_levels_of_separation"
                                } else {
                                        whereAthenaField <- "min_levels_of_separation"
                                }


                                ancestors <-
                                        leftJoin(.data = .data,
                                                 column = descendant_id_column,
                                                 athena_schema = athena_schema,
                                                 athena_table = "concept_ancestor",
                                                 athena_column = "descendant_concept_id",
                                                 where_athena_col = whereAthenaField,
                                                 where_athena_col_in = whereLevelIn,
                                                 verbose = verbose,
                                                 conn = conn,
                                                 render_sql = render_sql,
                                                 sleepTime = sleepTime,
                                                 ...)

                        } else {

                                ancestors <-
                                        leftJoin(.data = .data,
                                                 column = descendant_id_column,
                                                 athena_schema = athena_schema,
                                                 athena_table = "concept_ancestor",
                                                 athena_column = "descendant_concept_id",
                                                 verbose = verbose,
                                                 conn = conn,
                                                 render_sql = render_sql,
                                                 sleepTime = sleepTime,
                                                 ...)
                        }



                                ancestors_detail <-
                                        leftJoinConcept(ancestors %>%
                                                                dplyr::select(ancestor_concept_id),
                                                        athena_schema = athena_schema,
                                                        synonyms = FALSE,
                                                        verbose = verbose,
                                                        conn = conn,
                                                        render_sql = render_sql,
                                                        sleepTime = sleepTime,
                                                        ...) %>%
                                        dplyr::select(-ancestor_concept_id) %>%
                                        rubix::rename_all_with_prefix("ancestor_") %>%
                                        dplyr::distinct()


                                final_ancestors <-
                                        dplyr::left_join(ancestors,
                                                         ancestors_detail,
                                                         by = "ancestor_concept_id") %>%
                                        dplyr::select(-descendant_concept_id)


                                return(final_ancestors)

        }




#' LEFT JOIN the Concept Parent Table
#' @export

leftJoinFoChildren <-
        function(.data,
                 athena_schema,
                 parent_id_column = NULL,
                 render_sql = TRUE,
                 conn = NULL) {


                leftJoin(.data = .data,
                         column = parent_id_column,
                         athena_schema = athena_schema,
                         athena_table = "concept_parent",
                         athena_column = "parent_concept_id",
                         render_sql = render_sql,
                         conn = conn) %>%
                        dplyr::filter(parent_concept_id != child_concept_id)


        }




#' @title Left Join a data frame to the Concept Ancestor Table
#' @description FUNCTION_DESCRIPTION
#' @param .data PARAM_DESCRIPTION
#' @param athena_schema Default: 'public'
#' @param ancestor_id_column Default: NULL
#' @param whereLevelIn Default: NULL
#' @param whereLevelType Default: NULL
#' @param render_sql Default: TRUE
#' @param conn Default: NULL
#' @seealso
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{distinct}},\code{\link[dplyr]{mutate-joins}}
#'  \code{\link[rubix]{rename_all_with_prefix}}
#' @rdname leftJoinForDescendants
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr select distinct left_join
#' @importFrom rubix rename_all_with_prefix

leftJoinForDescendants <-
        function(.data,
                 athena_schema = "public",
                 ancestor_id_column = NULL,
                 whereLevelIn = NULL,
                 whereLevelType = NULL,
                 verbose = FALSE,
                 conn = NULL,
                 render_sql = FALSE,
                 sleepTime = 1,
                 ...) {

                        if (!is.null(whereLevelIn) && length(whereLevelType) != 1) {


                                warning("No 'whereLevelType'. Defaulting to 'max'")
                                whereLevelType <- "max"

                        }

                        if (!is.null(whereLevelIn)) {

                                if (whereLevelType == "max") {
                                        whereAthenaField <- "max_levels_of_separation"
                                } else {
                                        whereAthenaField <- "min_levels_of_separation"
                                }


                                descendants <-
                                        leftJoin(.data = .data,
                                                 column = ancestor_id_column,
                                                 athena_schema = athena_schema,
                                                 athena_table = "concept_ancestor",
                                                 athena_column = "ancestor_concept_id",
                                                 where_athena_col = whereAthenaField,
                                                 where_athena_col_in = whereLevelIn,
                                                 verbose = verbose,
                                                 conn = conn,
                                                 render_sql = render_sql,
                                                 sleepTime = sleepTime,
                                                 ...)

                        } else {

                                descendants <-
                                        leftJoin(.data = .data,
                                                 column = ancestor_id_column,
                                                 athena_schema = athena_schema,
                                                 athena_table = "concept_ancestor",
                                                 athena_column = "ancestor_concept_id",
                                                 verbose = verbose,
                                                 conn = conn,
                                                 render_sql = render_sql,
                                                 sleepTime = sleepTime,
                                                 ...)
                        }



                                descendants_detail <-
                                        leftJoinConcept(descendants %>%
                                                                dplyr::select(descendant_concept_id),
                                                        athena_schema = athena_schema,
                                                        synonyms = FALSE,
                                                        verbose = verbose,
                                                        conn = conn,
                                                        render_sql = render_sql,
                                                        sleepTime = sleepTime,
                                                        ...) %>%
                                        dplyr::select(-descendant_concept_id) %>%
                                        rubix::rename_all_with_prefix("descendant_") %>%
                                        dplyr::distinct()


                                final_descendants <-
                                        dplyr::left_join(descendants,
                                                         descendants_detail,
                                                         by = "descendant_concept_id") %>%
                                        dplyr::select(-ancestor_concept_id)


                                return(final_descendants)

        }




#' LEFT JOIN the Concept Parent Table
#' @export

leftJoinForParents <-
        function(.data,
                 athena_schema,
                 child_id_column = NULL,
                 render_sql = TRUE,
                 conn = NULL) {


                leftJoin(.data = .data,
                         column = child_id_column,
                         athena_schema = athena_schema,
                         athena_table = "concept_parent",
                         athena_column = "parent_concept_id",
                         render_sql = render_sql,
                         conn = conn) %>%
                        dplyr::filter(parent_concept_id != child_concept_id)

        }




#' Left Join Relationship
#' @export

leftJoinRelationship <-
        function(.data,
                 column = NULL,
                 athena_schema = "public",
                 render_sql = TRUE,
                 conn = NULL) {

                if (is.null(column)) {

                        column <- colnames(.data)[1]

                }



                .output1 <-
                        leftJoin(.data = .data %>%
                                         dplyr::select(all_of(column)),
                                 athena_schema = athena_schema,
                                 athena_table = "concept_relationship",
                                 athena_column = "concept_id_1",
                                 render_sql = render_sql,
                                 conn = conn) %>%
                        dplyr::filter(is.na(invalid_reason)) %>%
                        dplyr::select(-valid_start_date,
                                      -valid_end_date,
                                      -invalid_reason)

                .output1 <-
                        dplyr::left_join(.data,
                                         .output1,
                                         by = column)

                .output2 <-
                        leftJoinConcept(.data = .output1 %>%
                                                dplyr::select(concept_id_2),
                                        athena_schema = athena_schema,
                                        render_sql = render_sql,
                                        conn = conn) %>%
                                        dplyr::select(-concept_id_2) %>%
                                        rubix::rename_all_suffix(suffix = "_2")


                dplyr::left_join(.output1,
                                 .output2,
                                 by = "concept_id_2") %>%
                dplyr::select(!ends_with("_2"),
                              ends_with("_2"),
                              dplyr::everything())


        }




#' LEFT JOIN All Relatives
#' @import rubix
#' @import dplyr
#' @export

leftJoinRelatives <-
        function(.data,
                 athena_schema = "public",
                 id_column = NULL,
                 whereLevelIn = NULL,
                 whereLevelType = NULL,
                 render_sql = TRUE,
                 conn = NULL) {



                ancestors <-
                        leftJoinForAncestors(.data = .data,
                                             athena_schema = athena_schema,
                                             descendant_id_column = id_column,
                                             whereLevelIn = whereLevelIn,
                                             whereLevelType = whereLevelType,
                                             render_sql = render_sql,
                                             conn = conn)

                descendants <-
                        leftJoinForDescendants(.data = .data,
                                               athena_schema = athena_schema,
                                               ancestor_id_column = id_column,
                                               whereLevelIn = whereLevelIn,
                                               whereLevelType = whereLevelType,
                                               render_sql = render_sql,
                                               conn = conn)

                final <- list(A = ancestors,
                              D = descendants) %>%
                                rubix::map_names_set(function(x) x %>%
                                                             rubix::rename_all_remove(pattern = "ancestor_|descendant_")) %>%
                                dplyr::bind_rows(.id = "relative_type") %>%
                                rubix::rename_at_prefix(concept_id,
                                                        concept_name,
                                                        domain_id,
                                                        vocabulary_id,
                                                        concept_class_id,
                                                        standard_concept,
                                                        concept_code,
                                                        valid_start_date,
                                                        valid_end_date,
                                                        invalid_reason,
                                                        prefix = "relative_") %>%
                                dplyr::select(all_of(colnames(.data)),
                                              relative_type,
                                              min_levels_of_separation,
                                              max_levels_of_separation,
                                                    relative_concept_id,
                                                    relative_concept_name,
                                                    relative_domain_id,
                                                    relative_vocabulary_id,
                                                    relative_concept_class_id,
                                                    relative_standard_concept,
                                                    relative_concept_code,
                                                    relative_valid_start_date,
                                                    relative_valid_end_date,
                                                    relative_invalid_reason,
                                              dplyr::everything())


                return(final)
        }




#' @title Join a data frame with the Concept Synonym Table
#' @description
#' This function has better performance than the WHERE IN statement for larger searches. A table in the format of "v{unpunctuated timestamp}" is written to the local Athena. A join is performed with a concept table. The table is subsequently dropped. If a dataframe column is not provided as a the column to join the concept table on, the 1st column will be used by default.
#'
#' @param .data                 A data frame
#' @param column                Data frame column that the join will be performed on. If NULL, defaults to the column in position 1 of the data frame.
#' @param athena_schema         Schema of the OMOP Vocabulary Tables
#' @param verbose               If TRUE, prints whether the cache is being loaded or being actively queried in the Postgres database, Default: FALSE
#' @param conn                  PARAM_DESCRIPTION, Default: NULL
#' @param render_sql            If TRUE, will print the SQL to the console before executing. Default: FALSE
#' @param sleepTime             Argument in seconds passed to the `Sys.sleep()` function at the end of query, Default: 1
#' @param ...                   Additional arguments passed to the `queryAthena()` function.
#'
#' @return
#' A data frame
#'
#' @seealso
#'  \code{\link[dplyr]{select}}
#' @rdname leftJoinSynonymId
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr select


leftJoinSynonymId <-
    function(.data,
             column = NULL,
             athena_schema,
             verbose = FALSE,
             conn = NULL,
             render_sql = FALSE,
             sleepTime = 1) {


                            if (is.null(column)) {
                                column <- colnames(.data)[1]
                            }


                            if (column == "concept_id") {
                                stop("'column' parameter cannot be equal to 'concept_id'")
                            }


                            leftJoin(.data = .data,
                                      column = column,
                                      athena_schema = athena_schema,
                                      athena_table = "concept_synonym",
                                      athena_column = "concept_id",
                                      render_sql = render_sql,
                                      where_athena_col = "language_concept_id",
                                      where_athena_col_in = 4180186,
                                      verbose = verbose,
                                      conn = conn,
                                      sleepTime = sleepTime) %>%
                                    dplyr::select(-language_concept_id)

    }





#' List Join Tables
#' @import pg13
#' @export

lsJoinTables <-
        function(conn,
                 schema = NULL) {
                Tables <- pg13::lsTables(conn = conn,
                                         schema = schema)

                grep("^V[0-9]{14}$", Tables, value = TRUE, ignore.case = TRUE)


        }
