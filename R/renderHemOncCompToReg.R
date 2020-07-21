#' Render HemOnc Component to Regimen Query
#' @description This function renders the SQL that retrieves the HemOnc Regimens associated with 1 or more HemOnc Components
#' @return
#' @import SqlRender
#' @param component_concept_ids 1 or more HemOnc Component Concept Ids.
#' @export


renderHemOncCompToReg <-
    function(component_concept_ids,
             schema = NULL) {

                        if (is.null(schema)) {

                                schema <- "public"

                        }

                        base <- system.file(package = "chariot")
                        path <- paste0(base, "/sql/hemOncComponentToRegimen.sql")


                        sql_statement <-
                                SqlRender::render(SqlRender::readSql(sourceFile = path),
                                                  schema = schema,
                                                  component_concept_ids = component_concept_ids)

                        return(sql_statement)
    }
