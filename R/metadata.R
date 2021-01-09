#' @title
#' Read GitHub Wiki
#' @description
#' Read the tables from OHDI's GitHub Wiki at
#' \url{https://ohdsi.github.io/CommonDataModel/cdm60.html} to annotate
#' resultsets. This function is run on package load to cache the tables if they
#' are older than the expiration days.
#' @param expiration_days If the cached file was created longer than this time
#' period in days, the GitHub wiki is re-read and cached. Default: 180
#' @seealso
#'  \code{\link[R.cache]{findCache}}
#'  \code{\link[xml2]{read_xml}}
#'  \code{\link[rvest]{html_nodes}},\code{\link[rvest]{html_table}},\code{\link[rvest]{html_text}}
#' @rdname read_cdm_wiki_table
#' @export
#' @importFrom R.cache findCache
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_table html_text
read_cdm_wiki_table <-
  function(expiration_days = 180) {

    cache_file <-
    R.cache::findCache(key = list("read_cdm_wiki_table"),
                       dirs = "chariot_package")

    if (!is.null(cache_file)) {
    if (as.double(difftime(Sys.time(),file.info(cache_file)$mtime, units = "days")) >= 180) {

        response <- xml2::read_html("https://ohdsi.github.io/CommonDataModel/cdm60.html")

        data <-
          response %>%
          rvest::html_nodes("table") %>%
          rvest::html_table(fill = TRUE)

        names(data) <-
          response %>%
          rvest::html_nodes("h3") %>%
          rvest::html_text()

        lowLevelCache(data = data,
                      query = "read_cdm_wiki_table")

        return(data)

    } else {
       data <- lowLevelLoadCache(query = "read_cdm_wiki_table")

       data


    }
    } else {

      response <- xml2::read_html("https://ohdsi.github.io/CommonDataModel/cdm60.html")

      data <-
        response %>%
        rvest::html_nodes("table") %>%
        rvest::html_table(fill = TRUE)

      names(data) <-
        response %>%
        rvest::html_nodes("h3") %>%
        rvest::html_text()

      lowLevelCache(data = data,
                    query = "read_cdm_wiki_table")

      return(data)
    }
  }



#' @title
#' List the Common Data Model Tables
#' @description
#' List the names of the Common Data Model tables without the Vocabulary tables.
#' @rdname list_cdm_table_names
#' @export
list_cdm_table_names <-
  function() {
    c("PERSON", "OBSERVATION_PERIOD", "VISIT_OCCURRENCE", "VISIT_DETAIL", "CONDITION_OCCURRENCE", "DRUG_EXPOSURE", "PROCEDURE_OCCURRENCE", "DEVICE_EXPOSURE", "MEASUREMENT", "OBSERVATION", "NOTE", "NOTE_NLP", "SPECIMEN", "FACT_RELATIONSHIP", "SURVEY_CONDUCT", "LOCATION", "LOCATION_HISTORY", "CARE_SITE", "PROVIDER", "PAYER_PLAN_PERIOD", "COST", "DRUG_ERA", "DOSE_ERA", "CONDITION_ERA", "METADATA", "CDM_SOURCE")
  }


#' @title
#' List the Vocabulary Tables
#' @rdname list_vocab_table_names
#' @export
list_vocab_table_names <-
  function() {
    c("CONCEPT", "VOCABULARY", "DOMAIN", "CONCEPT_CLASS", "CONCEPT_RELATIONSHIP", "RELATIONSHIP", "CONCEPT_SYNONYM", "CONCEPT_ANCESTOR", "SOURCE_TO_CONCEPT_MAP", "DRUG_STRENGTH")
  }


#' @title
#' Get the CDM Data Dictionary
#' @seealso
#'  \code{\link[dplyr]{bind}}
#'  \code{\link[rubix]{format_colnames}}
#' @rdname get_cdm_data_dictionary
#' @export
#' @importFrom dplyr bind_rows
#' @importFrom rubix format_colnames
get_cdm_data_dictionary <-
  function() {
      omop_cdm_wiki <- read_cdm_wiki_table()
      cdm_table_names <- list_cdm_table_names()
      omop_cdm_wiki[cdm_table_names] %>%
        dplyr::bind_rows(.id = "table") %>%
        rubix::format_colnames()
  }

#' @title
#' Get the Concept Id Field Names for Each Table
#' @seealso
#'  \code{\link[rubix]{filter_at_grepl}},\code{\link[rubix]{split_deselect}}
#'  \code{\link[dplyr]{select}}
#'  \code{\link[purrr]{map}}
#' @rdname get_concept_id_fields
#' @export
#' @importFrom rubix filter_at_grepl split_deselect
#' @importFrom dplyr select
#' @importFrom purrr map
get_concept_id_fields <-
  function() {
    cdm_metadata <- get_cdm_data_dictionary()
    cdm_metadata %>%
      rubix::filter_at_grepl(col = cdm_field,
                             grepl_phrase = "concept_id") %>%
      dplyr::select(table, cdm_field) %>%
      rubix::split_deselect(col = table) %>%
      purrr::map(unlist) %>%
      purrr::map(unname)
  }

#' @title
#' Get the Vocabulary Data Dictionary
#' @seealso
#'  \code{\link[dplyr]{bind}}
#'  \code{\link[rubix]{format_colnames}}
#' @rdname get_vocab_data_dictionary
#' @export
#' @importFrom dplyr bind_rows
#' @importFrom rubix format_colnames
get_vocab_data_dictionary <-
  function() {
    omop_cdm_wiki <- read_cdm_wiki_table()
    table_names <- list_vocab_table_names()
    omop_cdm_wiki[table_names] %>%
      dplyr::bind_rows(.id = "Table") %>%
      rubix::format_colnames()
  }
