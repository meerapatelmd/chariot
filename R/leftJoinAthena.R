#' Join a dataframe object with Athena's concept table
#' @description This function has better performance than the WHERE IN statement for larger searches. A table in the format of "v{unpunctuated timestamp}" is written to the local Athena. A join is performed with a concept table. The table is subsequently dropped. If a dataframe column is not provided as a the column to join the concept table on, the 1st column will be used by default.
#' @param dataframe dataframe to join
#' @param column string of the column name to join on. If NULL, the 1st column is used.
#' @param athena_column name of column to join dataframe on. Defaults to concept ID.
#' @export

leftJoinAthena <-
    function(.data,
             column = NULL,
             athena_schema,
             athena_table,
             athena_column,
             where_athena_col = NULL,
             where_athena_col_in = NULL,
             override_cache = FALSE,
             print_sql = TRUE) {


                table_name <- paste0("v", stampede::stamp_this(without_punct = TRUE))

                if (is.null(column)) {
                    column <- colnames(.data)[1]
                }

                conn <- connectAthena()
                pg13::writeTable(conn = conn,
                                 schema = "public",
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
                                     joinType = "LEFT",
                                     caseInsensitive = FALSE,
                                     joinOnSchema = athena_schema,
                                     joinOnTableName = athena_table,
                                     joinOnColumn = athena_column,
                                     whereInField = where_athena_col,
                                     whereInVector = where_athena_col_in)

                if (print_sql) {

                        secretary::typewrite(sql_statement)

                }

                if (override_cache) {

                        resultset <-
                            query_athena(sql_statement = sql_statement,
                                         cache_resultset = FALSE)

                        cache_left_join(object=resultset,
                                        vector=.data %>%
                                            dplyr::select(all_of(column)) %>%
                                            unlist() %>%
                                            unname(),
                                        athena_table = athena_table,
                                        athena_column = athena_column,
                                        where_athena_col = where_athena_col,
                                        where_athena_col_equals = where_athena_col_in,
                                        omop=FALSE,
                                        omop_schema=NULL)

                } else {

                    cached_resultset <-
                        load_cached_left_join(vector = .data %>%
                                                  dplyr::select(all_of(column)) %>%
                                                  unlist() %>%
                                                  unname(),
                                              athena_table = athena_table,
                                              athena_column = athena_column,
                                              where_athena_col = where_athena_col,
                                              where_athena_col_equals = where_athena_col_in,
                                              omop=FALSE,
                                              omop_schema=NULL)

                    if (is.null(cached_resultset)) {

                            resultset <-
                                query_athena(sql_statement = sql_statement,
                                             cache_resultset = FALSE)

                            cache_left_join(object=resultset,
                                            vector=.data %>%
                                                dplyr::select(all_of(column)) %>%
                                                unlist() %>%
                                                unname(),
                                            athena_table = athena_table,
                                            athena_column = athena_column,
                                            where_athena_col = where_athena_col,
                                            where_athena_col_equals = where_athena_col_in,
                                            omop=FALSE,
                                            omop_schema=NULL)


                    } else {

                        resultset <- cached_resultset

                    }

                }

                drop_left_join_tables()
                return(resultset)
    }
