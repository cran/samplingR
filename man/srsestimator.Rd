% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/srsestimator.R
\name{srsestimator}
\alias{srsestimator}
\title{Simple Ramdom Sampling parameter estimation.}
\usage{
srsestimator(
  N,
  data,
  estimator = c("total", "mean", "proportion", "class total"),
  replace = FALSE,
  alpha
)
}
\arguments{
\item{N}{Number of instances of the data set.}

\item{data}{Sample of the data. It must only contain a single column of the
data to estimate.}

\item{estimator}{One of "total", "mean", "proportion", "class total". Default is "total".}

\item{replace}{Whether the sample has been taken with replacement or not.}

\item{alpha}{Optional value to calculate estimation error and build 1-alpha
confidence interval.}
}
\value{
A list containing different interest values:
\itemize{
\item estimator
\item variance
\item sampling.error
\item estimation.error
\item confint
}
}
\description{
Function to make estimations of diferent parameters based on
a Simple Random Sample.
}
\examples{
data<-rnorm(200, 100, 20)
sample<-data[samplingR::srs(200, 50)]
tau<-sum(data);tau
srsestimator(200, sample, "total", alpha=0.05)


mu<-mean(data);mu
srsestimator(200, sample, "mean", alpha=0.05)
}
