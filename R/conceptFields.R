#' List the Fields in the Concept Table
#' @export

conceptFields <-
        function(schema,
                 tableName,
                 conn = NULL) {

                .tableFields(schema = schema,
                             tableName = "concept",
                             conn = conn)
        }
