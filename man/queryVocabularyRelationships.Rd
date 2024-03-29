% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/query.R
\name{queryVocabularyRelationships}
\alias{queryVocabularyRelationships}
\title{FUNCTION_TITLE}
\usage{
queryVocabularyRelationships(
  vocabulary_id,
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
\item{vocabulary_id}{PARAM_DESCRIPTION}

\item{verbose}{PARAM_DESCRIPTION, Default: FALSE}

\item{conn}{PARAM_DESCRIPTION, Default: NULL}

\item{cache_only}{PARAM_DESCRIPTION, Default: FALSE}

\item{skip_cache}{PARAM_DESCRIPTION, Default: FALSE}

\item{override_cache}{PARAM_DESCRIPTION, Default: FALSE}

\item{render_sql}{PARAM_DESCRIPTION, Default: FALSE}

\item{sleepTime}{PARAM_DESCRIPTION, Default: 1}
}
\value{
OUTPUT_DESCRIPTION
}
\description{
FUNCTION_DESCRIPTION
}
\seealso{
\code{\link[SqlRender]{render}},\code{\link[SqlRender]{readSql}}

Other query functions: 
\code{\link{queryAncestors}()},
\code{\link{queryAthena}()},
\code{\link{queryCDM}()},
\code{\link{queryCode}()},
\code{\link{queryConceptClassRelationships}()},
\code{\link{queryConceptId}()},
\code{\link{queryDescendants}()},
\code{\link{queryRelationships}()},
\code{\link{querySynonyms}()},
\code{\link{query_athena}()}
}
\concept{query functions}
