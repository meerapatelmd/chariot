#' Render Vocabulary Table DDL
#' @import pg13
#' @import SqlRender
#' @export

renderVocabularyTableDDL <-
        function() {

                path <- pg13::sourceFilePath(instSubdir = "sql",
                                             FileName = "vocabularyTableDDL.sql",
                                             package = "chariot")

                SqlRender::render(SqlRender::readSql(sourceFile = path))


        }
