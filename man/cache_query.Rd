% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cache.R
\name{cache_query}
\alias{cache_query}
\title{Cache the results of a query}
\usage{
cache_query(object, key)
}
\arguments{
\item{object}{object to cache}

\item{key}{equivalent to the SQL query string.}
}
\value{
Invisibly returns the path to the cache file
}
\description{
This function uses the R.cache::saveCache function with the
destination directory by Package name "Athena".
}
