



lj_conceptId <-
        function(data,
                 column,
                 omopSchema,
                 writeSchema,
                 cast_to_varchar = FALSE,
                 render_sql = TRUE,
                 verbose = TRUE) {

                lj_con <-
                fantasia::join2_ff(omopTable = "concept",
                                   join_type = "LEFT")





                Args <- "concept_id"
                names(Args) <- column

                lj_con(data = data,
                        omopSchema = omopSchema,
                        omopTable = "concept",
                        Args,
                        writeSchema = writeSchema,
                        cast_to_varchar = cast_to_varchar,
                        case_insensitive = FALSE,
                        render_sql = render_sql,
                        verbose = verbose
                )



        }


test_f <-
        function(...) {
                list(...)
        }


test_f2 <-
        function(...) {
                list2(...)
        }

