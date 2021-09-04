require(magrittr)

#' @importFrom magrittr %>%

get_lateral_relationship_ids <-
  function() {
query_athena(
        sql_statement = "SELECT DISTINCT relationship_id FROM omop_vocabulary.concept_relationship WHERE relationship_id NOT IN ('Mapped from', 'Maps from', 'Concept replaced by', 'Concept replaces',  'Subsumes', 'Is a')") %>%
        dplyr::select(relationship_id) %>%
        unlist() %>%
        unname()

  }


#
#' @importFrom magrittr %>%

get_foreign_relationship_ids <-
  function() {
        query_athena(
                sql_statement = "SELECT DISTINCT relationship_id FROM omop_vocabulary.concept_relationship cr INNER JOIN omop_vocabulary.concept c1 ON cr.concept_id_1 = c1.concept_id INNER JOIN omop_vocabulary.concept c2 ON cr.concept_id_2 = c2.concept_id  WHERE c1.vocabulary_id = c2.vocabulary_id;") %>%
        dplyr::select(relationship_id) %>%
        unlist() %>%
        unname()
  }

#' @export
#' @importFrom magrittr %>%

relationship_classes <-
glue::glue(
"
ExplicitRelationshipTypes:
    Map:
      Values: ['Mapped from', 'Maps from']
    Update:
      Values: ['Concept replaced by', 'Concept replaces']
    Taxonomy:
      Values: ['Subsumes', 'Is a']
DerivedRelationshipTypes:
    Lateral: [{glue::glue_collapse(glue::single_quote(get_lateral_relationship_ids()), sep = ',')}]
    Foreign:  [{glue::glue_collapse(glue::single_quote(get_foreign_relationship_ids()), sep = ',')}]
    CrossDomain:
"
)

# yaml::yaml.load(string = relationship_classes)
