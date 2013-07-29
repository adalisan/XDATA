\name{temporal.normalize.stats}
\alias{temporal.normalize.stats}
\title{Normalize the maximum scan statistics at each time interval using the statistics most recent el-length time window}
\usage{
  temporal.normalize.stats(max.stats.list, el)
}
\arguments{
  \item{stats.list}{a named vector that contains the
  maximum scan statistics in the graph at each time
  interval and names in the named vector correspond to
  vertex ids.}

  \item{el}{length of time window}
}
\value{
  a vector of same length as stats.list. The first (el)
  values will be 0, since there is no el-length history to
  normalize with respect to
}
\description{
  Normalize the maximum scan statistics at each time
  interval using the statistics most recent el-length time
  window
}

