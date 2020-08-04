#' Combine Parents and Children Relationships
#' @import purrr
#' @import collapsibleTree
#' @import tibble
#' @import dplyr
#' @export

concept.tree <-
	function(concept_id,
	         parental_generations = 1,
	         child_generations = 1,
                override_cache = FALSE,
                verbose = FALSE,
                cache_resultset = TRUE,
                conn = NULL,
                render_sql = TRUE,
                schema,
                style) {


	        if (style == "bottleneck") {


        	        familyTree <-
        	                queryFamilyTree(concept_id = concept_id,
        	                                parental_generations = parental_generations,
        	                                child_generations = child_generations,
        	                                override_cache = override_cache,
        	                                verbose = verbose,
        	                                cache_resultset = cache_resultset,
        	                                conn = conn,
        	                                render_sql = render_sql,
        	                                schema = schema)


        	        familyTree <-
        	                familyTree %>%
        	                purrr::keep(~is.data.frame(.))


        	        if (length(familyTree) == 0) {

        	                stop("familyTree has 0 length")

        	        }

        	        # Creating starting node where parent is NA and child is concept_id
        	        if (length(unique(familyTree[[1]]$parent_concept_id)) == 1) {

        	                output0 <-
        	                        tibble::tibble(parent_concept_id = NA,
        	                                       child_concept_id = unique(familyTree[[1]]$parent_concept_id))


        	        } else {
        	                output0 <- list()
        	                output0[[1]] <-
                	                tibble::tibble(parent_concept_id = 00000,
                	                               child_concept_id = familyTree[[1]]$parent_concept_id) %>%
        	                        dplyr::distinct()

        	                output0[[2]] <-
        	                        tibble::tibble(parent_concept_id = NA,
        	                                       child_concept_id = 00000)

        	                output0 <-
        	                        output0[length(output0):1]

        	                output0 <- dplyr::bind_rows(output0)
        	        }

        	        output <- list()
        	        output[[1]] <- output0

        	        for (i in 1:length(familyTree)) {
        	                output[[i+1]] <-
        	                        familyTree[[i]]
        	        }

        	        output <- output %>%
        	                       dplyr::bind_rows()


        	        output <-
        	                output %>%
        	                dplyr::mutate_all(as.integer) %>%
        	                leftJoinConcept(column = "parent_concept_id") %>%
        	                dplyr::select(-parent_concept_id) %>%
        	                # rubix::rename_at_prefix(!child_concept_id,
        	                #                         prefix = "parent_") %>%
        	                mergeLabel(into = "Parent") %>%
        	                ids_to_integer() %>%
        	                leftJoinConcept(column = "child_concept_id") %>%
        	                dplyr::select(-child_concept_id) %>%
        	                mergeLabel(into = "Child") %>%
        	                dplyr::select(Parent = parent,
        	                              Child = Child)


        	        return(output)


	        } else {


	                familyTree <-
	                        queryFamilyTree(concept_id = concept_id,
	                                        parental_generations = parental_generations,
	                                        child_generations = 0,
	                                        override_cache = override_cache,
	                                        verbose = verbose,
	                                        cache_resultset = cache_resultset,
	                                        conn = conn,
	                                        render_sql = render_sql,
	                                        schema = schema)


	                unique_parents <- familyTree[[1]]$parent_concept_id %>%
	                                                unique()

	                familyTree2 <- list()
	                for (i in 1:length(unique_parents)) {

	                        unique_parent <- unique_parents[i]

	                        total_generations <- parental_generations+child_generations

	                        familyTree2[[i]] <-
	                                queryConceptChildren(parent_id = unique_parent,
	                                                     schema = schema,
	                                                     generations = total_generations-1)

	                        names(familyTree2[[i]]) <- 1:length(familyTree2[[i]])
	                        names(familyTree2)[i] <- unique_parent

	                }

	                familyTree <-
	                        familyTree2 %>%
	                        purrr::transpose() %>%
	                        purrr::map(dplyr::bind_rows)


	                if (length(unique(familyTree[[1]]$parent_concept_id)) == 1) {

	                        output0 <-
	                                tibble::tibble(parent_concept_id = NA,
	                                               child_concept_id = unique(familyTree[[1]]$parent_concept_id))


	                } else {

	                        output0 <- list()
	                        output0[[1]] <-
	                                tibble::tibble(parent_concept_id = 00000,
	                                               child_concept_id = familyTree[[1]]$parent_concept_id) %>%
	                                dplyr::distinct()

	                        output0[[2]] <-
	                                tibble::tibble(parent_concept_id = NA,
	                                               child_concept_id = 00000)

	                        output0 <-
	                                output0[length(output0):1]

	                        output0 <- dplyr::bind_rows(output0)

	                }

	                output <- list()
	                output[[1]] <- output0

	                for (i in 1:length(familyTree)) {
	                        output[[i+1]] <-
	                                familyTree[[i]]
	                }

	                output <- output %>%
	                        dplyr::bind_rows()

	                output <<- output

	                output <-
	                        output %>%
	                        dplyr::mutate_all(as.integer) %>%
	                        leftJoinConcept(column = "parent_concept_id") %>%
	                        dplyr::select(-parent_concept_id) %>%
	                        # rubix::rename_at_prefix(!child_concept_id,
	                        #                         prefix = "parent_") %>%
	                        mergeLabel(into = "Parent") %>%
	                        ids_to_integer() %>%
	                        leftJoinConcept(column = "child_concept_id") %>%
	                        dplyr::select(-child_concept_id) %>%
	                        mergeLabel(into = "Child") %>%
	                        dplyr::select(Parent = parent,
	                                      Child = Child)


	                return(output)



	        }

	}
