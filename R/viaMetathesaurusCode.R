#' @title Crosswalk a Search by the Metathesaurus CODE
#' @seealso
#'  \code{\link[SqlRender]{translate}}
#'  \code{\link[pg13]{buildQuery}},\code{\link[pg13]{query}}
#'  \code{\link[stringr]{str_replace}}
#'  \code{\link[preQL]{connectMySQL5.5}},\code{\link[preQL]{dcMySQL5.5}}
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[dplyr]{mutate-joins}},\code{\link[dplyr]{filter}}
#'  \code{\link[rubix]{filter_for}}
#' @rdname viaMetathesaurusCode
#' @export
#' @importFrom SqlRender translate
#' @importFrom pg13 buildQuery query
#' @importFrom stringr str_replace_all
#' @importFrom preQL connectMySQL5.5 dcMySQL5.5
#' @importFrom tibble tibble
#' @importFrom dplyr left_join filter
#' @importFrom rubix filter_for

viaMetathesaurusCode <-
    function(phrase,
             caseInsensitive = TRUE,
             conn = NULL,
             athena_schema,
             type = c("like", "exact"),
             verbose = FALSE,
             cache_resultset = TRUE,
             override_cache = FALSE,
             render_sql = TRUE,
             umls_conn = NULL,
             umls_username = Sys.getenv("umls_username"),
             umls_password = Sys.getenv("umls_password")) {

            sql_statement <-
            SqlRender::translate(
                    sql =
                    pg13::buildQuery(schema = "",
                                     tableName = "MRCONSO",
                                     whereInField = "STR",
                                     whereInVector = phrase,
                                     caseInsensitive = caseInsensitive),
                    targetDialect = "oracle") %>%
                    stringr::str_replace_all(pattern = "[ ]{1,}[.]{1}|[;]{1}$", " ")

            if (is.null(umls_conn)) {

                    conn <- preQL::connectMySQL5.5(dbname = "umls",
                                                   username = umls_username,
                                                   password = umls_password)

                    resultset  <- pg13::query(conn = conn,
                                              sql_statement = sql_statement)

                    preQL::dcMySQL5.5(conn = conn)

            } else {
                    resultset  <- pg13::query(conn = umls_conn,
                                              sql_statement = sql_statement)
            }


            vocabMatrix <-
                    tibble::tibble(SAB = c("SNOMEDCT_US", "LNC", "RXNORM"),
                                   vocabulary_id = c("SNOMED", "LOINC", "RxNorm"))


            output <-
                    dplyr::left_join(resultset,
                                     vocabMatrix,
                                     by = "SAB") %>%
                    dplyr::filter(!is.na(vocabulary_id))


            if (!is.null(conn)) {


                    conn <- connectAthena()
                    resultset <-queryCode(code = output$CODE,
                              schema = athena_schema,
                              caseInsensitive = caseInsensitive,
                              verbose = verbose,
                              cache_resultset = cache_resultset,
                              override_cache = override_cache,
                              conn = conn,
                              render_sql = render_sql,
                              type = "exact") %>%
                            rubix::filter_for(vocabulary_id,
                                              inclusion_vector = vocabMatrix$vocabulary_id)

                    dcAthena(conn = conn)


            } else {
                    resultset <-queryCode(code = output$CODE,
                                          schema = athena_schema,
                                          caseInsensitive = caseInsensitive,
                                          verbose = verbose,
                                          cache_resultset = cache_resultset,
                                          override_cache = override_cache,
                                          conn = conn,
                                          render_sql = render_sql,
                                          type = type) %>%
                            rubix::filter_for(vocabulary_id,
                                              inclusion_vector = vocabMatrix$vocabulary_id)
            }


            return(resultset)

    }
