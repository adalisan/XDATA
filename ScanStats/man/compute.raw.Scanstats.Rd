\name{compute.raw.Scanstats}
\alias{compute.raw.Scanstats}
\title{compute the "raw" local scan statistics of all the graphs in the time series}
\usage{
  compute.raw.Scanstats(Graph.List, k = 1,
    is.weighted.graph = FALSE)
}
\arguments{
  \item{Graph.List}{Time series of graphs as an R list}

  \item{k}{The radius of the neighborhood of the vertex to
  be considered for scan statistic}
}
\value{
  The raw statistics as a list of named vectors. The names
  of the vectors are the vector ids.
}
\description{
  compute the "raw" local scan statistics of all the graphs
  in the time series
}

