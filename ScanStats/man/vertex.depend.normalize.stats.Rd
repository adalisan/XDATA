\name{vertex.depend.normalize.stats}
\alias{vertex.depend.normalize.stats}
\title{Normalize the scan statistics for each vertex separately using the statistics most recent tau-length time window}
\usage{
  vertex.depend.normalize.stats(stats.list, tau = 0,
    us.T.them.F = TRUE, TSG = NULL, k = 1)
}
\arguments{
  \item{stats.list}{a list of named vectors where each
  named vector in the list contain statistics at one time
  interval and names in the named vectors correspond to
  vertex ids.}

  \item{tau}{length of time window}

  \item{us.T.them.F}{logical variable to choose computation
  of the `us` statistic (if TRUE) or `them` statistic (if
  FALSE)}

  \item{TSG}{the time series of graphs the scan statistics
  were computed from which is necessary if `them` statistic
  is being computed}

  \item{k}{The radius of the neighborhood of the vertex to
  be considered for scan statistic}
}
\value{
  a list with the same structure (list of named vectors) as
  stats.list. The first tau elements will be 0, since there
  is no tau-length history to normalize with respect to.
}
\description{
  Normalize the scan statistics for each vertex separately
  using the statistics most recent tau-length time window
}

