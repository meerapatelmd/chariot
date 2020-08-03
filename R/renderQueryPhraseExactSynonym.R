#' Render Query Concept Synonym Table
#' @import pg13
#' @import stringr
#' @export


renderQueryPhraseExactSynonym <-
        function(schema,
                 phrase,
                 caseInsensitive = TRUE) {


                if (caseInsensitive) {

                                sourceFile <- pg13::sourceFilePath(instSubdir = "sql",
                                                                   FileName = "queryLowerPhraseExactSynonym.sql",
                                                                   package = "chariot")


                } else {


                                sourceFile <- pg13::sourceFilePath(instSubdir = "sql",
                                                     FileName = "queryPhraseExactSynonym.sql",
                                                     package = "chariot")

                }


                SqlRender::render(SqlRender::readSql(sourceFile = sourceFile),
                                  schema = schema,
                                  phrase = phrase)

        }
