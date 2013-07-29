\name{local.scan.igraph}
\alias{local.scan.igraph}
\title{Calculate local scan statistics for each vertex in a graph}
\usage{
  local.scan.igraph(g, gp = NULL, k = 1, mode = "out",
    FUN = ecount)
}
\arguments{
  \item{g}{igraph object or matrix.}

  \item{gp}{igraph object or matrix. It's typically at
  t=t-1, for "them" statistics.}

  \item{k}{radii of the neighborhood.}

  \item{mode}{one of "in", "out", "total".}

  \item{FUN}{scan function. It's one of "ecount" or
  "vcount".}
}
\value{
  a vector of scan statistics, one for each vertex in the
  graph.
}
\description{
  Calculate local scan statistics for each vertex in a
  graph
}

