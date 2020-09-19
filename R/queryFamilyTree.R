#' Combine Parents and Children Relationships
#' @import purrr
#' @export

queryFamilyTree <-
	function(concept_id,
	         schema,
	         parental_generations = 1,
	         child_generations = 1,
	         verbose = FALSE,
	         cache_resultset = TRUE,
	         override_cache = FALSE,
	         conn = NULL,
	         render_sql = FALSE,
	         sleepTime = 1,
	         ...) {


	        if (parental_generations != 0) {

        	        parents <- queryConceptParent(child_id = concept_id,
        	                                      schema = schema,
        	                                      generations = parental_generations,
        	                                      verbose = verbose,
        	                                      cache_resultset = cache_resultset,
        	                                      override_cache = override_cache,
        	                                      conn = conn,
        	                                      render_sql = render_sql,
        	                                      sleepTime = sleepTime,
        	                                      ...)
	        } else {
	                parents <- NULL
	        }

	        if (child_generations != 0) {

        	        children <- queryConceptChildren(parent_id = concept_id,
        	                                         schema = schema,
        	                                         generations = child_generations,
        	                                         verbose = verbose,
        	                                         cache_resultset = cache_resultset,
        	                                         override_cache = override_cache,
        	                                         conn = conn,
        	                                         render_sql = render_sql,
        	                                         sleepTime = sleepTime,
        	                                         ...)

	        } else {

	                children <- NULL


	        }



        	        final <- c(parents,
        	                   children) %>%
        	                purrr::keep(~is.data.frame(.))

        	        return(final)

             }
