#' Render SQL Statement for Concept Synonyms
#' @description Get all the synonyms for a concept by concept_id.
#' @param schema If NULL, defaults to the public schema.
#' @import SqlRender
#' @export

renderSynonyms <-
        function(concept_id,
                 schema = NULL,
                 language_concept_id = 4180186) {

                if (is.null(schema)) {

                        schema <- "public"

                }

                base <- system.file(package = "chariot")
                path <- paste0(base, "/sql")

                SqlRender::render(SqlRender::readSql(paste0(path, "/synonyms.sql")),
                                  concept_id = concept_id,
                                  schema = schema,
                                  language_concept_id = language_concept_id)


        }
