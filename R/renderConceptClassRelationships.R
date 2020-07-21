#' Query Concept Class Relationships
#' @description This function retrieves the Subject-Predicate-Object triplet from the CONCEPT_RELATIONSHIP table between 2 vocabularies.
#' @return A dataframe with 3 columns: 1) concept_class_id of vocabulary_1 with name as "{vocabulary_id_1}_concept_class_id" 2) relationship_id from the CONCEPT_RELATIONSHIP table, and 3) concept_class_id of vocabulary_2 with name as "{vocabulary_id_2}_concept_class_id".
#' @import SqlRender
#' @param vocabulary_id_1 single vocabulary_id for the first vocabulary (Subject)
#' @param vocabulary_id_2 single vocabulary_id for the second vocabulary (Object)
#' @export


renderConceptClassRelationships <-
    function(vocabulary_id_1,
             vocabulary_id_2,
             schema = NULL) {

                        if (is.null(schema)) {

                                schema <- "public"

                        }

                        base <- system.file(package = "chariot")
                        path <- paste0(base, "/sql/conceptClassRelationship.sql")

                        #Create prefixes for concept_class_columns
                        concept_class_id_1 <- paste0(tolower(vocabulary_id_1), "_concept_class_id")
                        concept_class_id_2 <- paste0(tolower(vocabulary_id_2), "_concept_class_id")

                        sql_statement <-
                                SqlRender::render(SqlRender::readSql(sourceFile = path),
                                                  schema = schema,
                                                  concept_class_id_1 = concept_class_id_1,
                                                  concept_class_id_2 = concept_class_id_2,
                                                  vocabulary_id_1 = vocabulary_id_1,
                                                  vocabulary_id_2 = vocabulary_id_2)

                        return(sql_statement)
    }
