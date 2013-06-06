\name{reproduceAnalysis}
\alias{reproduceAnalysis}
\title{Reproduce the analysis}
\usage{
  reproduceAnalysis(step = "all", verbose = TRUE,
    browse = TRUE)
}
\arguments{
  \item{step}{If \code{all} then it reproduces all the
  steps. Other options are \code{EDA}, \code{pred},
  \code{Q1}, \code{Q2} and \code{Q3}.}

  \item{verbose}{If \code{TRUE} then progress messages will
  be printed along the way.}

  \item{browse}{If \code{TRUE} browser windows are opened
  after the completition of each step.}
}
\value{
  The directory \code{fitbitResults} inside your working
  directory with all the analysis steps reproduced.
}
\description{
  Reproduces all the analysis steps for Russo's JHSPH
  Biostat Qualifying exam 2013. To do so it will create a
  directory called \code{fitbitResults} in your working
  directory.
}
\details{
  For maximum cool factor, this package requires the
  version of \code{markdown} that is available from GitHub,
  which sadly has the same version number as the version
  from CRAN so it won't be detected through the usual
  means.
}
\examples{
reproduceAnalysis(step="EDA")
}
\references{
  knitr bootstrap html format from
  https://github.com/jimhester/knitr_bootstrap
}

