#' Search CDM instance for a Concept Id
#' @importFrom DatabaseConnector dbListTables
#' @importFrom DatabaseConnector dbListFields
#' @importFrom DatabaseConnector dbGetQuery
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @importFrom dplyr distinct
#' @export

findConceptId <-
    function(conn,
             concept_id,
             schema = NULL) {


            cdm <-
            pg13::lsTables(conn = conn,
                           schema = schema )


            output <- tibble::tibble()
            while (length(cdm) > 0) {

                    cdm_table <- cdm[1]

                    cdm_table_fields <-
                            pg13::lsFields(conn = conn,
                                           tableName = cdm_table)


                    cdm_table_cid_fields <-
                            grep("concept_id", cdm_table_fields, ignore.case = TRUE,
                                 value = TRUE)


                    while (length(cdm_table_cid_fields) > 0) {


                            cdm_table_cid_field <- cdm_table_cid_fields[1]


                            # sql <- paste0("SELECT * FROM ", cdm_table, " WHERE ", cdm_table_cid_field, " = ", concept_id, " LIMIT 1")

                            sql <-
                            pg13::buildQuery(schema = schema,
                                             tableName = cdm_table,
                                             whereInField = cdm_table_cid_field,
                                             whereInVector = concept_id,
                                             caseInsensitive = FALSE,
                                             n = 1,
                                             n_type = "limit")


                            resultset <- pg13::query(conn = conn,
                                                     sql_statement = sql)


                            if (nrow(resultset) > 0) {

                                    output <-
                                        dplyr::bind_rows(output,
                                                         tibble::tibble(Table = cdm_table,
                                                                Field = cdm_table_cid_field))

                            }


                            cdm_table_cid_fields <- cdm_table_cid_fields[-1]


                }

                cdm <- cdm[-1]


            }
            output %>%
                dplyr::distinct()

    }
