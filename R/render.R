#' Render Concept Class Relationships
#' @description This function renders the SQL that retrieves the Subject-Predicate-Object triplet from the CONCEPT_RELATIONSHIP table between 2 vocabularies.
#' @return A dataframe with 3 columns: 1) concept_class_id of vocabulary_1 with name as "{vocabulary_id_1}_concept_class_id" unless vocabulary_id_2 is NULL, in which case it will be concept_class_id_1 2) relationship_id from the CONCEPT_RELATIONSHIP table, and 3) concept_class_id of vocabulary_2 with name as "{vocabulary_id_2}_concept_class_id" unless vocabulary_id_2 is NULL, in which case it will be concept_class_id_2.
#' @import SqlRender
#' @param vocabulary_id_1 single vocabulary_id for the first vocabulary (Subject)
#' @param vocabulary_id_2 single vocabulary_id for the second vocabulary (Object). If vocabulary_id_2 is NULL, vocabulary_id_2 is set to vocabulary_id_1.
#' @export


renderConceptClassRelationships <-
    function(vocabulary_id_1,
             vocabulary_id_2 = NULL,
             vocab_schema = "omop_vocabulary") {

                        base <- system.file(package = "chariot")
                        path <- paste0(base, "/sql/conceptClassRelationship.sql")

                        # If vocabulary_id_2 is NULL, the relationships within vocabulary_id_1 are reported and if vocabulary_id_2 is not NULL, the relationships between the 2 vocabularies are reported

                        if (!is.null(vocabulary_id_2)) {

                                        ## CREATE VARIABLES FOR SQL RENDER
                                        #Create prefixes for concept_class_columns
                                        concept_class_id_1 <- paste0(tolower(vocabulary_id_1), "_concept_class_id")
                                        concept_class_id_2 <- paste0(tolower(vocabulary_id_2), "_concept_class_id")

                                        #Add single quotes for vocabularies
                                        vocabulary_id_1 <- paste0("'", vocabulary_id_1, "'")
                                        vocabulary_id_2 <- paste0("'", vocabulary_id_2, "'")


                        } else {

                                        ## CREATE VARIABLES FOR SQL RENDER
                                        concept_class_id_1 <- "concept_class_id_1"
                                        concept_class_id_2 <- "concept_class_id_2"
                                        vocabulary_id_1 <- paste0("'", vocabulary_id_1, "'")
                                        vocabulary_id_2 <- vocabulary_id_1


                        }


                                SqlRender::render(SqlRender::readSql(sourceFile = path),
                                                  schema = vocab_schema,
                                                  concept_class_id_1 = concept_class_id_1,
                                                  concept_class_id_2 = concept_class_id_2,
                                                  vocabulary_id_1 = vocabulary_id_1,
                                                  vocabulary_id_2 = vocabulary_id_2)


    }




#' Render HemOnc Component to Regimen Query
#' @description This function renders the SQL that retrieves the HemOnc Regimens associated with 1 or more HemOnc Components
#' @return
#' @import SqlRender
#' @param component_concept_ids 1 or more HemOnc Component Concept Ids.
#' @export


renderHemOncCompToReg <-
    function(component_concept_ids,
             vocab_schema = "omop_vocabulary") {


                        base <- system.file(package = "chariot")
                        path <- paste0(base, "/sql/hemOncComponentToRegimen.sql")


                                SqlRender::render(SqlRender::readSql(sourceFile = path),
                                                  schema = vocab_schema,
                                                  component_concept_ids = component_concept_ids)

    }








#' Render HemOnc Component to Regimen Query
#' @description This function renders the SQL that retrieves the HemOnc Regimens associated with 1 or more HemOnc Components
#' @return
#' @import SqlRender
#' @param component_concept_ids 1 or more HemOnc Component Concept Ids.
#' @export


renderHemOncRegToAntineoplastics <-
    function(regimen_concept_ids,
             vocab_schema = NULL) {

                        if (is.null(vocab_schema)) {

                                vocab_schema <- "public"

                        }

                        base <- system.file(package = "chariot")
                        path <- paste0(base, "/sql/hemOncRegimenToHasAntineoplastics.sql")


                        sql_statement <-
                                SqlRender::render(SqlRender::readSql(sourceFile = path),
                                                  schema = vocab_schema,
                                                  regimen_concept_ids = regimen_concept_ids)

                        return(sql_statement)
    }








#' Render SQL to Query for Ancestors
#' @import pg13
#' @export

renderQueryAncestors <-
        function(descendant_concept_ids,
                 vocab_schema,
                 min_levels_of_separation = NULL,
                 max_levels_of_separation = NULL) {


                sql_statement <-
                        stringr::str_remove_all(
                                pg13::buildQuery(schema = vocab_schema,
                                                 tableName = "concept_ancestor",
                                                 whereInField = "descendant_concept_id",
                                                 whereInVector = descendant_concept_ids,
                                                 caseInsensitive = FALSE),
                                pattern = "[;]{1}$")

                if (!is.null(min_levels_of_separation)) {

                        sql_statement <-
                                pg13::concatWhereConstructs(sql_statement,
                                                            pg13::constructWhereIn(field = "min_levels_of_separation",
                                                                                   vector = min_levels_of_separation))

                }

                if (!is.null(max_levels_of_separation)) {

                        sql_statement <-
                                pg13::concatWhereConstructs(sql_statement,
                                                            pg13::constructWhereIn(field = "max_levels_of_separation",
                                                                                   vector = max_levels_of_separation))

                }

                pg13::terminateBuild(sql_statement = sql_statement)


        }




#' Render SQL to Query for Descendants
#' @import pg13
#' @export

renderQueryDescendants <-
        function(ancestor_concept_ids,
                 vocab_schema,
                 min_levels_of_separation = NULL,
                 max_levels_of_separation = NULL) {


                sql_statement <-
                        stringr::str_remove_all(
                                pg13::buildQuery(schema = vocab_schema,
                                                 tableName = "concept_ancestor",
                                                 whereInField = "ancestor_concept_id",
                                                 whereInVector = ancestor_concept_ids,
                                                 caseInsensitive = FALSE),
                                pattern = "[;]{1}$")

                if (!is.null(min_levels_of_separation)) {

                        sql_statement <-
                                pg13::concatWhereConstructs(sql_statement,
                                                            pg13::constructWhereIn(field = "min_levels_of_separation",
                                                                                   vector = min_levels_of_separation))

                }

                if (!is.null(max_levels_of_separation)) {

                        sql_statement <-
                                pg13::concatWhereConstructs(sql_statement,
                                                            pg13::constructWhereIn(field = "max_levels_of_separation",
                                                                                   vector = max_levels_of_separation))

                }

                pg13::terminateBuild(sql_statement = sql_statement)


        }




#' Render Query Concept Synonym Table
#' @import pg13
#' @import stringr
#' @export


renderQueryPhraseExactSynonym <-
        function(vocab_schema,
                 phrase,
                 caseInsensitive = TRUE) {


                phrase <- paste0("'", phrase, "'")


                base <- system.file(package = "chariot")
                path <- paste0(base, "/sql")

                if (caseInsensitive) {


                                path_to_sourceFile <-paste0(path, "/queryLowerPhraseExactSynonym.sql")
                                SqlRender::render(SqlRender::readSql(sourceFile = path_to_sourceFile),
                                                  schema = vocab_schema,
                                                  phrase = tolower(phrase))


                } else {

                        path_to_sourceFile <-paste0(path, "/queryPhraseExactSynonym.sql")
                        SqlRender::render(SqlRender::readSql(sourceFile = path_to_sourceFile),
                                          schema = vocab_schema,
                                          phrase = phrase)

                }


        }




#' Render Query Concept Synonym Table
#' @import pg13
#' @import stringr
#' @export


renderQueryPhraseLikeSynonym <-
        function(vocab_schema,
                 phrase,
                 caseInsensitive = TRUE) {




                base <- system.file(package = "chariot")
                path <- paste0(base, "/sql")

                if (caseInsensitive) {


                                path_to_sourceFile <-paste0(path, "/queryLowerPhraseLikeSynonym.sql")
                                SqlRender::render(SqlRender::readSql(sourceFile = path_to_sourceFile),
                                                  schema = vocab_schema,
                                                  phrase = tolower(phrase))


                } else {

                        path_to_sourceFile <-paste0(path, "/queryPhraseLikeSynonym.sql")
                        SqlRender::render(SqlRender::readSql(sourceFile = path_to_sourceFile),
                                          schema = vocab_schema,
                                          phrase = phrase)

                }


        }




#' Render SQL Statement for Concept Synonyms
#' @description Get all the synonyms for a concept by concept_id.
#' @param vocab_schema If NULL, defaults to the public schema.
#' @import SqlRender
#' @export

renderSynonyms <-
        function(concept_id,
                 vocab_schema = NULL,
                 language_concept_id = 4180186) {

                if (is.null(vocab_schema)) {

                        vocab_schema <- "public"

                }

                base <- system.file(package = "chariot")
                path <- paste0(base, "/sql")

                SqlRender::render(SqlRender::readSql(paste0(path, "/synonyms.sql")),
                                  concept_id = concept_id,
                                  schema = vocab_schema,
                                  language_concept_id = language_concept_id)


        }




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
