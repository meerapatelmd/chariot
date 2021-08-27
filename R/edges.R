



n4j_query <-
        function(cypher_qry,
                 conn,
                neo4j_key = "dbms-78e47f6b-c070-4cb1-8339-6caf203aec0f") {

                if (missing(conn)) {

                conn <- neo4jconn::start_neo4j(db_key = neo4j_key)
                on.exit(expr = neo4jconn::stop_neo4j(conn = conn),
                        add = TRUE,
                        after = TRUE)
                }


                neo4jshell::neo4j_query(con = conn$conn_details,
                                        qry = cypher_qry)



        }
