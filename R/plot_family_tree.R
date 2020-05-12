#' Plot the ancestry of a concept
#' @description Plot ancestry  out of the dataframe returned from derive_family_tree()
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @importFrom rubix filter_at_grepl
#' @importFrom ggenealogy plotAncDes
#' @export

plot_family_tree <-
    function(concept_id, family_tree_df, generations_before, generations_after) {
        gg_df <- family_tree_df
        concept <-
            gg_df %>%
            tidyr::pivot_longer(cols = c("parent",
                                         "child"),
                                names_to = "concept_level",
                                values_to = "concept") %>%
            dplyr::select(concept) %>%
            dplyr::distinct() %>%
            rubix::filter_at_grepl(col = concept,
                                   grepl_phrase = paste0(" ", concept_id, " ")) %>%
            unlist() %>%
            unname()
        
        ggenealogy::plotAncDes(concept, gg_df, generations_before, generations_after)
    }