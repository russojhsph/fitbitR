fitbitR
=======

JHSPH Biostat qualifying exam 2013 take home: analyzing Fitbit activity data in 5 min intervals.


## Installation instructions

```S
library(devtools)
## Required for knitr bootstrap html reports.
install_github(username='rstudio', repo='markdown')

## This is the main package.
install_github("fitbitR", "russojhsph")
```

## Shiny Application


```S
## Either run from fitbitR
library(fitbitR)
fitbitShine()

## Or from the web
library(shiny)
runUrl("https://github.com/russojhsph/fitbitR/archive/master.zip",
subdir = "inst/fitbitShine")
```
