#' @title
#' Process Incoming Concepts
#'
#' @param data          Dataframe
#' @param concept_col   Pointer to the column with the concepts to map
#'
#' @name process_map_input_functions
#'
#' @keywords internal
NULL


#' @title
#' Make Modifications to Concepts for SQL querying
#'
#' @description
#' A series of modifications are required for concepts to avoid errors when using them as arguments for SQL querying such as the single quote in "Kaposi's sarcoma" requiring a replacement with an additional "'" escape character, resulting in a string that reads "Kaposi''s sarcoma".
#'
#' @inheritParams process_map_input_functions
#'
#' @seealso
#'  \code{\link[dplyr]{tidyeval-compat}},\code{\link[dplyr]{mutate_all}}
#'  \code{\link[stringr]{str_replace}},\code{\link[stringr]{str_remove}}
#' @rdname prime_concepts
#' @export
#' @importFrom dplyr as_label mutate_at
#' @importFrom stringr str_replace_all str_remove_all


prime_concepts <-
  function(data,
           concept_col) {
    new_col_name <- paste0(dplyr::as_label(concept_col), " Primed")


    data %>%
      dplyr::mutate_at(
        dplyr::vars({{ concept_col }}),
        ~ stringr::str_remove_all(., "[?]{1}$|[']")
      )
  }


#' @title
#' Process the Words Column from the Concept Column
#'
#' @inheritParams process_map_input_functions
#' @param sep           Argument passed to the tidyr::separate_rows, Default: ' '
#' @param word_nchar    Number of theshold characters for words to be over to be included in the output, Default: 3
#'
#' @rdname process_words
#' @export
#' @importFrom dplyr left_join mutate filter mutate_at filter_at as_label
#' @importFrom tidyr separate_rows
#' @importFrom stringr str_remove_all


process_words <-
  function(data,
           concept_col,
           sep = " ",
           word_nchar = 3) {

    concept_col <- dplyr::enquo(concept_col)
    new_col_name <- paste0(dplyr::as_label(concept_col), " Words")


    dplyr::left_join(
      data,
      data %>%
        dplyr::mutate({{ new_col_name }} := {{ concept_col }}) %>%
        tidyr::separate_rows({{ new_col_name }}, sep = " ") %>%
        dplyr::filter({{ concept_col }} != {{ new_col_name }}) %>%
        dplyr::mutate_at(
          dplyr::vars({{ new_col_name }}),
          stringr::str_remove_all,
          "^[[:punct:]]{1,}|[[:punct:]]{1,}$"
        ) %>%
        dplyr::filter_at(
          dplyr::vars({{ new_col_name }}),
          dplyr::all_vars(nchar(.) > word_nchar)
        )
    )
  }


#' @title
#' Process a Concept Column for Parenthetical Phrases
#'
#' @details
#' To process a Concept Column with multiple embedded parenthetical phrases, see \code{\link{process_first_parentheses}} and \code{\link{process_last_parentheses}}.
#'
#' @inheritParams process_map_input_functions
#'
#' @rdname process_parentheses
#'
#' @export
#'
#' @importFrom dplyr left_join mutate mutate_all filter mutate_at filter_at enquo as_label
#' @importFrom tidyr separate_rows
#' @importFrom stringr str_remove_all

process_parentheses <-
  function(data,
           concept_col) {
    col <- dplyr::enquo(concept_col)

    new_col_name1 <- paste0(dplyr::as_label(col), " Front of Parentheses")
    new_col_name2 <- paste0(dplyr::as_label(col), " Inside Parentheses")
    new_col_name3 <- paste0(dplyr::as_label(col), " Behind Parentheses")

    dplyr::left_join(
      data,
      data %>%
        dplyr::mutate(Parentheses := !!col) %>%
        tidyr::extract(
          col = Parentheses,
          into = c(new_col_name1, new_col_name2, new_col_name3),
          regex = "(^.*?)[(]{1}(.*?)[)]{1}(.*$)",
          remove = FALSE
        ) %>%
        dplyr::select(-Parentheses)
    ) %>%
      dplyr::mutate_all(trimws) %>%
      normalize_nas()
  }

#' @title
#' Process a Concept Column for the First Set of Parentheses
#'
#' @details
#' This is the same function as \code{\link{process_parentheses}} except that the Concept Column name is appended with "1st Parentheses" instead of "Parentheses" to accommodate a call to   \code{link{process_last_parentheses}}.
#'
#' @inheritParams process_map_input_functions
#' @rdname process_first_parentheses
#' @export
#' @importFrom dplyr left_join mutate mutate_all filter mutate_at filter_at enquo as_label
#' @importFrom tidyr separate_rows
#' @importFrom stringr str_remove_all

process_first_parentheses <-
  function(data,
           concept_col) {
    col <- dplyr::enquo(concept_col)

    new_col_name1 <- paste0(dplyr::as_label(col), " Front of 1st Parentheses")
    new_col_name2 <- paste0(dplyr::as_label(col), " Inside 1st Parentheses")
    new_col_name3 <- paste0(dplyr::as_label(col), " Behind 1st Parentheses")

    dplyr::left_join(
      data,
      data %>%
        dplyr::mutate(Parentheses := !!col) %>%
        tidyr::extract(
          col = Parentheses,
          into = c(new_col_name1, new_col_name2, new_col_name3),
          regex = "(^.*?)[(]{1}(.*?)[)]{1}(.*$)",
          remove = FALSE
        ) %>%
        dplyr::select(-Parentheses)
    ) %>%
      dplyr::mutate_all(trimws) %>%
      normalize_nas()
  }


#' @title
#' Process a Concept Column for the Last Set of Parentheses
#'
#' @details
#' This function differs from \code{\link{process_parentheses}} and \code{link{process_last_parentheses}} by performing a greedy regex match for parentheses in cases where there are multiple parenthetical statements within the concept column.
#'
#' @inheritParams process_map_input_functions
#' @rdname process_last_parentheses
#' @export
#' @importFrom dplyr left_join mutate mutate_all filter mutate_at filter_at enquo as_label
#' @importFrom tidyr separate_rows
#' @importFrom stringr str_remove_all

process_last_parentheses <-
  function(data,
           concept_col) {
    col <- dplyr::enquo(concept_col)

    new_col_name1 <- paste0(dplyr::as_label(col), " Front of nth Parentheses")
    new_col_name2 <- paste0(dplyr::as_label(col), " Inside nth Parentheses")
    new_col_name3 <- paste0(dplyr::as_label(col), " Behind nth Parentheses")

    dplyr::left_join(
      data,
      data %>%
        dplyr::mutate(Parentheses := !!col) %>%
        tidyr::extract(
          col = Parentheses,
          into = c(new_col_name1, new_col_name2, new_col_name3),
          regex = "(^.*)[(]{1}(.*?)[)]{1}(.*$)",
          remove = FALSE
        ) %>%
        dplyr::select(-Parentheses)
    ) %>%
      dplyr::mutate_all(trimws) %>%
      normalize_nas()
  }


#' @title
#' Identify Words Unique to the Concept in the Concept Set
#'
#' @inheritParams process_map_input_functions
#' @param words_col   Words column produced by \code{\link{process_words}}
#'
#' @seealso
#'  \code{\link[dplyr]{tidyeval-compat}},\code{\link[dplyr]{mutate-joins}},\code{\link[dplyr]{count}},\code{\link[dplyr]{filter}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{select}}
#' @rdname process_unique_words
#' @export
#' @importFrom dplyr enquo as_label left_join count filter mutate select

process_unique_words <-
  function(data,
           words_col) {
    words_col <- dplyr::enquo(words_col)
    new_col_name <- paste0(dplyr::as_label(words_col), " Unique")

    dplyr::left_join(
      data,
      data %>%
        dplyr::count({{ words_col }}) %>%
        dplyr::filter(n == 1) %>%
        dplyr::mutate({{ new_col_name }} := {{ words_col }}) %>%
        dplyr::select(-n)
    )
  }

#' @title
#' Identify the Longest Word in the Concept
#'
#' @inheritParams process_map_input_functions
#' @param words_col   Words column produced by \code{\link{process_words}}
#' @seealso
#'  \code{\link[dplyr]{tidyeval-compat}},\code{\link[dplyr]{mutate-joins}},\code{\link[dplyr]{group_by_all}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{filter}},\code{\link[dplyr]{select}},\code{\link[dplyr]{rename}}
#' @rdname process_longest_words
#' @export
#' @importFrom dplyr enquo as_label left_join group_by_at mutate filter select rename

process_longest_words <-
  function(data, concept_col, words_col) {

    # concept_col <- dplyr::enquo(concept_col)
    # words_col <- dplyr::enquo(words_col)
    # new_col_name <- paste0(dplyr::as_label(concept_col), " Longest Word")
    #
    # dplyr::left_join(data,
    #                  data %>%
    #                      dplyr::group_by_at(dplyr::vars({{concept_col}})) %>%
    #                      dplyr::mutate(longest_word = max(nchar(!!words_col))) %>%
    #                      dplyr::filter(nchar(!!words_col) == longest_word) %>%
    #                      dplyr::select(-longest_word) %>%
    #                      dplyr::rename({{new_col_name}} := !!words_col))
    #

    concept_col <- dplyr::enquo(concept_col)
    words_col <- dplyr::enquo(words_col)
    # words_col <- dplyr::enquo(words_col)
    new_col_name <- paste0(dplyr::as_label(concept_col), " Longest Word")

    dplyr::left_join(
      data,
      data %>%
        dplyr::mutate(nchar_words := nchar(!!words_col)) %>%
        dplyr::group_by_at(dplyr::vars({{ concept_col }})) %>%
        dplyr::mutate(longest_word = max(nchar_words)) %>%
        dplyr::filter(nchar_words == longest_word) %>%
        dplyr::ungroup() %>%
        dplyr::select(-longest_word) %>%
        dplyr::rename({{ new_col_name }} := {{ words_col }}) %>%
        dplyr::select(-nchar_words)
    )
  }

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param concept_col PARAM_DESCRIPTION
#' @param words_col PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[dplyr]{tidyeval-compat}},\code{\link[dplyr]{mutate-joins}},\code{\link[dplyr]{group_by_all}},\code{\link[dplyr]{arrange_all}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{group_by}}
#' @rdname rank_word_length
#' @export
#' @importFrom dplyr enquo as_label left_join group_by_at arrange_at mutate ungroup

rank_word_length <-
  function(data, concept_col, words_col) {
    new_col_prefix <- paste0(dplyr::as_label(words_col), " Length Rank")

    dplyr::left_join(
      data,
      data %>%
        dplyr::group_by_at(dplyr::vars({{ concept_col }})) %>%
        dplyr::arrange_at(dplyr::vars({{ words_col }}), ~ desc(nchar(.)), .by_group = TRUE) %>%
        dplyr::mutate(Rank := 1:n()) %>%
        dplyr::ungroup() %>%
        tidyr::pivot_wider(
          names_from = Rank,
          names_prefix = new_col_prefix,
          values_from = {{ words_col }}
        )
    )
  }


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param concept_col PARAM_DESCRIPTION
#' @param words_col PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[dplyr]{tidyeval-compat}},\code{\link[dplyr]{count}},\code{\link[dplyr]{mutate-joins}},\code{\link[dplyr]{group_by_all}},\code{\link[dplyr]{arrange_all}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{group_by}},\code{\link[dplyr]{select}}
#' @rdname rank_word_frequency
#' @export
#' @importFrom dplyr enquo as_label count left_join group_by_at arrange_at mutate ungroup select

rank_word_frequency <-
  function(data, concept_col, words_col) {
    concept_col <- dplyr::enquo(concept_col)
    words_col <- dplyr::enquo(words_col)

    new_col_prefix <- paste0(dplyr::as_label(words_col), " Frequency Rank")

    frequency_df <-
      data %>%
      dplyr::count(!!words_col)

    dplyr::left_join(
      data,
      data %>%
        dplyr::left_join(frequency_df) %>%
        dplyr::group_by_at(dplyr::vars(!!concept_col)) %>%
        dplyr::arrange_at(dplyr::vars(n), .by_group = TRUE) %>%
        dplyr::mutate(Rank := 1:n()) %>%
        dplyr::ungroup() %>%
        dplyr::select(-n) %>%
        tidyr::pivot_wider(
          names_from = Rank,
          names_prefix = new_col_prefix,
          values_from = !!words_col
        )
    )
  }
