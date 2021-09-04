omop.concept <- setRefClass(
        "omop.concept",
        fields =
                list(
                concept_id = "numeric",
                concept_name = "character",
                concept_synonym_names = "character",
                domain_id = "character",
                vocabulary_id = "character",
                concept_class_id = "character",
                standard_concept = "character",
                concept_code = "character",
                valid_start_date = "Date",
                valid_end_date = "Date",
                invalid_reason = "character"),
        methods =
                list(
                        show =
                                function(x) print(sprintf("[%s] [%s] %s %s [%s %s] [%s]",
                                                    ifelse(is.na(.self$invalid_reason), "V", .self$invalid_reason),
                                                    ifelse(is.na(.self$standard_concept), "N", .self$standard_concept),
                                                    .self$concept_id,
                                                    .self$concept_name,
                                                    .self$vocabulary_id,
                                                    .self$concept_code,
                                                    .self$domain_id,
                                                    .self$concept_class_id))
                )
)


make_omop_concept <-
        function(concept.data) {

                new("omop.concept",
                    concept_id = concept.data$concept_id,
                    concept_name = concept.data$concept_name,
                    domain_id = concept.data$domain_id,
                    vocabulary_id = concept.data$vocabulary_id,
                    concept_class_id = concept.data$concept_class_id,
                    standard_concept = concept.data$standard_concept,
                    concept_code = concept.data$concept_code,
                    valid_start_date = concept.data$valid_start_date,
                    valid_end_date = concept.data$valid_end_date,
                    invalid_reason = concept.data$invalid_reason)


        }




