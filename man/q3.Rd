\name{q3}
\alias{q3}
\title{Infer differences between weekdays and weekends in patterns of activity}
\usage{
  q3(data, by, plot = TRUE)
}
\arguments{
  \item{data}{A data set just like the one produced from
  \link{preprocess}.}

  \item{by}{Must be either \code{weekend} or \code{dow}
  (day of the week).}

  \item{plot}{If \code{TRUE} the pattern is plotted.}
}
\value{
  The fitted GAM model.
}
\description{
  Infers whether there are differences between weekdayas
  and weekends in patterns of activity.
}
\details{
  Check \link{reproduceAnalysis} with \code{step} set to
  \code{Q3} for more details on inferring the differences
  in activity patterns between weekends and weekdays (or by
  day of the week)
}
\examples{
input <- fitbitData
data <- preprocess(input)
q3.weekend <- q3(data, "weekend")
q3.dow <- q3(data, "dow")
}

