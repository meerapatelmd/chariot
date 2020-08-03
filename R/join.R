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
             render_sql = TRUE,
             conn = NULL) {


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
                                                    cache_resultset = FALSE,
                                                    render_sql = render_sql)



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
                                                 cache_resultset = FALSE,
                                                 render_sql = render_sql,
                                                 conn = conn)

                        dropJoinTables(conn = conn,
                                       schema = athena_schema)
                }

                return(resultset)
    }

