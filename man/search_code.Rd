% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/x_search.R
\name{search_code}
\alias{search_code}
\title{Search for a Code in the Concept Table}
\usage{
search_code(
  code_pattern,
  ignore.case = FALSE,
  perl = FALSE,
  fixed = FALSE,
  useBytes = FALSE,
  vocab_schema = "omop_vocabulary",
  limit = NULL,
  conn,
  conn_fun = "connectAthena()",
  cache_only = FALSE,
  skip_cache = FALSE,
  override_cache = FALSE,
  verbose = TRUE,
  render_sql = TRUE,
  sleepTime = 1
)
}
\arguments{
\item{code_pattern}{Regex pattern of code}

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

\item{verbose}{If TRUE, prints loading and querying operations
messages to the console. Default: FALSE}

\item{render_sql}{If TRUE, the SQL will be printed back in the
console prior to execution. Default: FALSE}

\item{sleepTime}{Argument for `Sys.sleep()` in between queries to
allow for halting function execution, especially in cases where other chariot
functions are executing multiple queries in succession and require
cancellation.}
}
\value{
resultset as a dataframe with all column types as character and trimmed white space
}
\description{
Search for a Code in the Concept Table
}
\examples{
if(interactive()){
 search_code(code_pattern = "^C40")
 }
}
