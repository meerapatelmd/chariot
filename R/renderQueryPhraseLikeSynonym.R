#' Render Query Concept Synonym Table
#' @import pg13
#' @import stringr
#' @export


renderQueryPhraseLikeSynonym <-
        function(schema,
                 phrase,
                 caseInsensitive = TRUE) {




                base <- system.file(package = "chariot")
                path <- paste0(base, "/sql")

                if (caseInsensitive) {


                                path_to_sourceFile <-paste0(path, "/queryLowerPhraseLikeSynonym.sql")
                                SqlRender::render(SqlRender::readSql(sourceFile = path_to_sourceFile),
                                                  schema = schema,
                                                  phrase = tolower(phrase))


                } else {

                        path_to_sourceFile <-paste0(path, "/queryPhraseLikeSynonym.sql")
                        SqlRender::render(SqlRender::readSql(sourceFile = path_to_sourceFile),
                                          schema = schema,
                                          phrase = phrase)

                }


        }
