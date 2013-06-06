\name{q1}
\alias{q1}
\title{Estimate the average number of steps taken per day}
\usage{
  q1(data, method, acf = TRUE)
}
\arguments{
  \item{data}{A data set just like the one produced from
  \link{preprocess}.}

  \item{method}{Must be \code{mean} or \code{auto.arima}.}

  \item{acf}{If \code{TRUE} and \code{method} is set to
  \code{auto.arima} then an autocorrelation plot of the
  data binned by date is plotted.}
}
\value{
  A vector with the estimate, it's SE, and a 95% CI
  assuming using a t-dist. If \code{method} is set to
  \code{auto.arima} then it's a list where the first
  element is the vector previously described and the second
  element is the fitted object from the auto.arima model.
}
\description{
  Estimates the average number of steps taken per day
  naively (sample mean) or using an ARIMA model with the
  data binned by date.
}
\details{
  Check \link{reproduceAnalysis} with \code{step} set to
  \code{Q1} for more details on estimating the steps taken
  per day.
}
\examples{
input <- fitbitData
data <- preprocess(input)
datap <- fitbitPred(data, method="lm")
q1(data, "mean")
q1(datap, "mean")
q1(data, "auto.arima")
q1(datap, "auto.arima")
}
\seealso{
  \link{binDay}
}

