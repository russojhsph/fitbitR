#' Explore your own data using insight gained from fitbitR
#'
#' Opens the Shiny application that allows you to visualizer your own Fitbit activity (number of steps) data using results from this analysis project.
#'
#' @param port The TCP port that the application should listen on. Defaults to port 8100. (Taken from \link{shiny::runApp})
#' @param launch.browser If \code{TRUE}, the system's default web browser will be launched automatically after the app is started.
#'
#' @details This function runs the Shiny application including in this package. It is basically a wrapper for \link{shiny::runApp}.
#'
#' @return The Shiny application is opened in your browser. To stop the function from running you have to cancel the command using ctrl + c or the equivalent shortcut in your system.
#'
#' @examples
#' fitbitShine()
#' ## Remember to cancel the command to get back to the R console once you are done analyzing your own data.
#' @export
#' @references For more information on what a Shiny app is check http://www.rstudio.com/shiny/

fitbitShine <- function(port = 8100L, launch.browser = TRUE) {
	# Required packages	
	require(shiny)
	
	## Locate the Shiny code
	root <- "fitbitShine"
	srcdir <- system.file(root, package="fitbitR")
	
	## Run the shiny application
	runApp(appDir=srcdir, port=port, launch.browser=launch.browser)
	
}
