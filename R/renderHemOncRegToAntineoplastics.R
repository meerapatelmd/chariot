#' Render HemOnc Component to Regimen Query
#' @description This function renders the SQL that retrieves the HemOnc Regimens associated with 1 or more HemOnc Components
#' @return
#' @import SqlRender
#' @param component_concept_ids 1 or more HemOnc Component Concept Ids.
#' @export


renderHemOncRegToAntineoplastics <-
    function(regimen_concept_ids,
             schema = NULL) {

                        if (is.null(schema)) {

                                schema <- "public"

                        }

                        base <- system.file(package = "chariot")
                        path <- paste0(base, "/sql/hemOncRegimenToHasAntineoplastics.sql")


                        sql_statement <-
                                SqlRender::render(SqlRender::readSql(sourceFile = path),
                                                  schema = schema,
                                                  regimen_concept_ids = regimen_concept_ids)

                        return(sql_statement)
    }




