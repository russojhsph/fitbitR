\name{q2}
\alias{q2}
\title{Infer the average activity pattern by day}
\usage{
  q2(data, method, plot = TRUE)
}
\arguments{
  \item{data}{A data set just like the one produced from
  \link{preprocess}.}

  \item{method}{Must be \code{mean} or \code{gam}.}

  \item{plot}{If \code{TRUE} the pattern is plotted.}
}
\value{
  A list with two elements: \code{Mean} with the average
  activity pattern and \code{SE} which is the SE.
}
\description{
  Infers the average activity pattern over time (within a
  day).
}
\details{
  Check \link{reproduceAnalysis} with \code{step} set to
  \code{Q2} for more details on inferring the average
  activity pattern.
}
\examples{
input <- fitbitData
data <- preprocess(input)
q2.mean <- q2(data, "mean")
q2.gam <- q2(data, "gam")
}

