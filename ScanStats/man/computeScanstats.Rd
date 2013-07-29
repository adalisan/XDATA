\name{computeScanstats}
\alias{computeScanstats}
\title{Compute the (us and/or them) scan statistics for a time series of graphs}
\usage{
  computeScanstats(TheBig.TS.Graph, k = 1, us.stats = TRUE,
    them.stats = TRUE, tau = 3, el = 3,
    is.weighted.graph = FALSE)
}
\arguments{
  \item{TheBig.TS.Graph}{The Time series of graphs as a
  list of igraph objects}

  \item{k}{the local neighborhood radius to consider for
  the scan statistic.}

  \item{us.stats}{Compute "us" statistics}

  \item{them.stats}{Compute "them" statistics}

  \item{tau}{Consider last 3 timesteps for vertex-dependent
  normalization of local scan statistics}

  \item{el}{Consider last 3 timesteps for temporal
  normalization of maximum scan statistics}
}
\value{
  subset of the list of transactions
}
\description{
  Compute the (us and/or them) scan statistics for a time
  series of graphs
}

