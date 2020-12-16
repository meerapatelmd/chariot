library(tidyverse)
library(chariot)
target_concept_class <- 21601386
antineoplastics_class <-
        get_concept(concept_id = target_concept_class,
                    vocab_schema = "omop_vocabulary")
conn <- chariot::connectAthena()
preplot_df <-
        plot_atc_classification(conn = conn,
                                         concept_class_obj = antineoplastics_class,
                                         skip_plot = TRUE)
# Add RxNorm Ingredients to Plot
preplot_df2 <-
        preplot_df %>%
        extract(col = child,
                into = "atc_concept_id",
                regex = "(^.*?) .*$",
                remove = FALSE)

preplot_df3 <-
        leftJoinForDescendants(data = preplot_df2,
                               vocab_schema = "omop_vocabulary",
                               ancestor_id_column = "atc_concept_id",
                               verbose = TRUE,
                               render_sql = TRUE,
                               write_schema = "patelm9",
                               conn = conn)

# Select subset to append later
rxnorm <-
        preplot_df3 %>%
        dplyr::select(child,
                      max_levels_of_separation,
                      min_levels_of_separation,
                      descendant_concept_id,
                      descendant_concept_name:last_col()) %>%
        rubix::filter_at_grepl(col = descendant_vocabulary_id,
                               grepl_phrase = "rxnorm") %>%
        rubix::filter_at_grepl(col = "descendant_concept_class_id",
                               grepl_phrase = "ingredient") %>%
        dplyr::filter_at(dplyr::vars(dplyr::contains("levels")), dplyr::any_vars(. == 0)) %>%
        dplyr::select(-dplyr::contains("levels")) %>%
        dplyr::distinct() %>%
        dplyr::mutate(parent = child,
                      child = paste0(descendant_concept_id, " ", descendant_concept_name)) %>%
        dplyr::mutate_all(as.character) %>%
        rubix::rename_all_remove(pattern = "descendant_") %>%
        tidyr::pivot_longer(cols = !c(parent,child),
                            names_to = "attribute",
                            values_to = "attribute_value",
                            values_drop_na = TRUE) %>%
        tidyr::unite(col = tooltip,
                     attribute,
                     attribute_value,
                     sep = ": ",
                     remove = TRUE,
                     na.rm = TRUE) %>%
        dplyr::distinct() %>%
        dplyr::group_by(parent, child) %>%
        dplyr::summarize_at(dplyr::vars(tooltip), ~paste(., collapse = "<br>")) %>%
        dplyr::ungroup() %>%
        dplyr::distinct()

final <-
        dplyr::bind_rows(preplot_df,
                         rxnorm)

final[is.na(final)] <- "NA"
final <-
        final %>%
        dplyr::mutate(old_color = color)
final$color <- factor(final$color)
levels(final$color) <- colorspace::diverging_hcl(n = length(levels(final$color)))
final$color <- as.character(final$color)

final <-
        final %>%
        dplyr::select(parent,
                      child,
                      color,
                      tooltip)

final$parent[final$parent %in% "NA"] <- NA_character_

p <- collapsibleTree::collapsibleTreeNetwork(df = final,
                                        tooltipHtml = "tooltip",
                                        fill = "color")
htmlwidgets::saveWidget(widget = p,
                        file = "cancer_classification.html")

chariot::dcAthena(conn = conn)
