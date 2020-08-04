#' Combine Parents and Children Relationships
#' @import purrr
#' @export

queryFamilyTree <-
	function(concept_id,
			parental_generations = 1,
			 child_generations = 1,
             override_cache = FALSE,
             verbose = FALSE,
             cache_resultset = TRUE,
             conn = NULL,
             render_sql = TRUE,
             schema) {


	        if (parental_generations != 0) {

        	        parents <- queryConceptParent(child_id = concept_id,
        	                                      generations = parental_generations,
        	                                      override_cache = override_cache,
        	                                      verbose = verbose,
        	                                      cache_resultset = cache_resultset,
        	                                      conn = conn,
        	                                      render_sql = render_sql,
        	                                      schema = schema)
	        } else {
	                parents <- NULL
	        }

	        if (child_generations != 0) {

        	        children <- queryConceptChildren(parent_id = concept_id,
        	                                         generations = child_generations,
        	                                         override_cache = override_cache,
        	                                         verbose = verbose,
        	                                         cache_resultset = cache_resultset,
        	                                         conn = conn,
        	                                         render_sql = render_sql,
        	                                         schema = schema)

	        } else {

	                children <- NULL


	        }



        	        final <- c(parents,
        	                   children) %>%
        	                purrr::keep(~is.data.frame(.))

        	        return(final)

             }
