\name{temp.norm}
\alias{temp.norm}
\title{Perform temporal normalization on time series of (normalized) local scan statistics}
\usage{
  temp.norm(stat, tau = 1, ell = 0)
}
\arguments{
  \item{stat}{#vertex x #time matrix of local statistics}

  \item{ell}{time window}
}
\value{
  a vector of #time temporally normilized scan statistics
}
\description{
  Perform temporal normalization on time series of
  (normalized) local scan statistics
}

