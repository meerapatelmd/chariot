#' Filter Merged Concepts
#' @param .data dataframe that contains the merged concept column
#' @param merge_col column of merged concepts
#' @param ... arguments for filter function using concept table fields
#' @importFrom dplyr enquo
#' @importFrom dplyr filter
#' @export


filter_merge <- 
    function(.data, 
             merge_col,
             ...) {
        
            merge_col <- dplyr::enquo(merge_col) 
            
            .data %>%
                unmerge_concepts(concept_col = !!merge_col) %>%
                dplyr::filter(...) %>%
                merge_concepts(into = !!merge_col)
    }
