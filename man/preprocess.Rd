\name{preprocess}
\alias{preprocess}
\title{Pre-process the raw data}
\usage{
  preprocess(input)
}
\arguments{
  \item{input}{Must be a matrix such as the one in
  \link{fitbitData}.}
}
\value{
  A data.frame with 7 columns: \code{Interval} specifying
  the 5 minute interval, \code{Date} specifying the date,
  \code{nSteps} which is the number of steps taken in that
  interval, the day of the week stored in \code{Day},
  whether it's a weekend or weekday stored in
  \code{Weekend}, the actual time stamp in \code{Time}, and
  the interval transformed to a scale from 0 to 287 stored
  in \code{IntS}.
}
\description{
  Re-arranges the data for further analyses.
}
\examples{
input <- fitbitData
data <- preprocess(input)
head(data)
summary(data)
}
\seealso{
  \link{fitbitData}
}

