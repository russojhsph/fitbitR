\name{fitbitPred}
\alias{fitbitPred}
\title{Predict values for the missing data.}
\usage{
  fitbitPred(data, method)
}
\arguments{
  \item{data}{A data set just like the one produced from
  \link{preprocess}.}

  \item{method}{Must be \code{poisson}, \code{lm},
  \code{means} or \code{overall-mean}.}
}
\value{
  A data.frame where missing observations have been
  replaced with the predicted values.
}
\description{
  Predicts the number of steps for the missing intervals.
}
\details{
  Check \link{reproduceAnalysis} with \code{step} set to
  \code{pred} for more details on the exploration of the
  prediction methods. Four of them were chosen from there
  and are implemented in \code{fitbitPred} although you
  should be warned that the root mean squared prediction
  error is around 100.
}
\examples{
input <- fitbitData
data <- preprocess(input)
head(data)
summary(data)
new <- fitbitPred(data, method="lm")
summary(new)
}
\seealso{
  \link{fitbitData}, \link{preprocess}
}

