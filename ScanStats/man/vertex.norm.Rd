\name{vertex.norm}
\alias{vertex.norm}
\title{Perform vertex normalization on time series of local scan statistics}
\usage{
  vertex.norm(stat, tau = 1)
}
\arguments{
  \item{stat}{#vertex x #time matrix or #vertex x (tau+1) x
  #time array of local statistics}

  \item{tau}{time window}
}
\value{
  a #vertex x #time normilized matrix of scan statistics
}
\description{
  Perform vertex normalization on time series of local scan
  statistics
}

