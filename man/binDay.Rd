\name{binDay}
\alias{binDay}
\title{Binned the data by date}
\usage{
  binDay(data)
}
\arguments{
  \item{data}{A data set just like the one produced from
  \link{preprocess}.}
}
\value{
  A data.frame with 4 columns: Date, nSteps (the number of
  steps taken that day), Day: day of the week, and Weekend:
  whether it's a weekend or weekday.
}
\description{
  Bins the data by date.
}
\examples{
input <- fitbitData
data <- preprocess(input)
datab <- binDay(data)
summary(datab)
}
\seealso{
  \link{q1}
}

