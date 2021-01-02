#' @noRd

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
      lowLevelLoadCache(query = "read_cdm_wiki_table")



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



list_cdm_table_names <-
  function() {
    c("PERSON", "OBSERVATION_PERIOD", "VISIT_OCCURRENCE", "VISIT_DETAIL", "CONDITION_OCCURRENCE", "DRUG_EXPOSURE", "PROCEDURE_OCCURRENCE", "DEVICE_EXPOSURE", "MEASUREMENT", "OBSERVATION", "NOTE", "NOTE_NLP", "SPECIMEN", "FACT_RELATIONSHIP", "SURVEY_CONDUCT", "LOCATION", "LOCATION_HISTORY", "CARE_SITE", "PROVIDER", "PAYER_PLAN_PERIOD", "COST", "DRUG_ERA", "DOSE_ERA", "CONDITION_ERA", "METADATA", "CDM_SOURCE")
  }


list_vocab_table_names <-
  function() {
    c("CONCEPT", "VOCABULARY", "DOMAIN", "CONCEPT_CLASS", "CONCEPT_RELATIONSHIP", "RELATIONSHIP", "CONCEPT_SYNONYM", "CONCEPT_ANCESTOR", "SOURCE_TO_CONCEPT_MAP", "DRUG_STRENGTH")
  }


get_cdm_data_dictionary <-
  function() {
      omop_cdm_wiki <- read_cdm_wiki_table()
      cdm_table_names <- list_cdm_table_names()
      omop_cdm_wiki[cdm_table_names] %>%
        dplyr::bind_rows(.id = "table") %>%
        rubix::format_colnames()
  }

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

get_vocab_data_dictionary <-
  function() {
    omop_cdm_wiki <- read_cdm_wiki_table()
    table_names <- list_vocab_table_names()
    omop_cdm_wiki[table_names] %>%
      dplyr::bind_rows(.id = "Table") %>%
      rubix::format_colnames()
  }
