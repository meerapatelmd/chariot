% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/query.R
\name{queryCode}
\alias{queryCode}
\title{FUNCTION_TITLE}
\usage{
queryCode(
  code,
  vocab_schema,
  caseInsensitive = TRUE,
  limit = NULL,
  verbose = FALSE,
  conn = NULL,
  cache_only = FALSE,
  skip_cache = FALSE,
  override_cache = FALSE,
  render_sql = FALSE,
  sleepTime = 1
)
}
\arguments{
\item{verbose}{If TRUE, prints loading and querying operations
messages to the console. Default: FALSE}

\item{conn}{Connection object. If provided, diverts queries
to the connection instead of the local Athena instance without caching
features.}

\item{cache_only}{Loads from the cache and does not query the
database. A NULL object is returned if a resultset was not cached.}

\item{skip_cache}{Skip the caching altogether and directly
query the database.}

\item{override_cache}{If TRUE, the cache will not be loaded and will
be overwritten by a new query. For override_cache to take effect,
skip_cache should be FALSE.}

\item{render_sql}{If TRUE, the SQL will be printed back in the
console prior to execution. Default: FALSE}

\item{sleepTime}{Argument for `Sys.sleep()` in between queries to
allow for halting function execution, especially in cases where other chariot
functions are executing multiple queries in succession and require
cancellation.}
}
\value{
a [tibble][tibble::tibble-package]
}
\description{
FUNCTION_DESCRIPTION
}
\seealso{
\code{\link[pg13]{build_query}}

Other query functions: 
\code{\link{queryAncestors}()},
\code{\link{queryAthena}()},
\code{\link{queryCDM}()},
\code{\link{queryConceptClassRelationships}()},
\code{\link{queryConceptId}()},
\code{\link{queryDescendants}()},
\code{\link{queryRelationships}()},
\code{\link{querySynonyms}()},
\code{\link{queryVocabularyRelationships}()},
\code{\link{query_athena}()}
}
\concept{query functions}
