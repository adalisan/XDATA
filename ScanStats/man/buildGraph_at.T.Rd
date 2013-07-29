\name{buildGraph_at.T}
\alias{buildGraph_at.T}
\title{Build a Time series of graphs of length numt
from a list of interactions}
\usage{
  buildGraph_at.T(trans.list, numt,
    div.by.equaltime.T.by.equalcount.F,
    build.weighted.graph)
}
\arguments{
  \item{trans.list}{List of all transactions, with as a
  data frame with "from","to","time" and "amount"}

  \item{numt}{: length of the time series}

  \item{div.by.equaltime.T.by.equalcount.F}{divide the
  transactions into timesteps by equal intervals of time or
  by equal number of transactions}

  \item{build.weighted.graph}{}
}
\value{
  A time series of graphs where each graph at a time
  interval is the largest connected component of the
  unweighted graph that is composed of the transactions at
  that time interval
}
\description{
  Build a Time series of graphs of length numt from a list
  of interactions
}

