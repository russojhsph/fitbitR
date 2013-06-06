#' Infer differences between weekdays and weekends in patterns of activity
#'
#' Infers whether there are differences between weekdayas and weekends in patterns of activity.
#'
#' @param data A data set just like the one produced from \link{preprocess}.
#' @param by Must be either \code{weekend} or \code{dow} (day of the week).
#' @param plot If \code{TRUE} the pattern is plotted.
#'
#' @details Check \link{reproduceAnalysis} with \code{step} set to \code{Q3} for more details on inferring the differences in activity patterns between weekends and weekdays (or by day of the week)
#'
#' @return The fitted GAM model.
#'
#' @examples 
#' input <- fitbitData
#' data <- preprocess(input)
#' q3.weekend <- q3(data, "weekend")
#' q3.dow <- q3(data, "dow")
#' @export

q3 <- function(data, by, plot=TRUE) {
	if(!by %in% c("weekend", "dow")) stop("'by' is incorrectly specified.")
	## Libs required
	require(mgcv)
	
	if(plot) {
		## Plotting software
		require(ggplot2)
	}	
	
	## Get the estimate and SD
	if(by == "weekend") {
		## Fit the GAM model
		fit <- gam(nSteps ~ s(IntS, bs="cr") + Weekend, data=data, family=quasipoisson)
		
		if(plot) {
			## Make the plot
			p <- ggplot(data, aes(x = Interval, y = nSteps, group = Weekend)) + geom_point(alpha = 1/4, na.rm = TRUE, colour = "orange") + facet_grid(~Weekend) + stat_smooth(method = gam, formula = y ~ s(x, bs = "cr"), na.rm = TRUE, family = quasipoisson) + ylab("Number of steps")
			print(p)
		}
			
	} else if (by == "dow") {
		## Fit the GAM model
		fit <- gam(nSteps ~ s(IntS, bs="cr") + Day, data=data, family=quasipoisson)
		
		if(plot) {
			## Make the plot
			p <- ggplot(data, aes(x = Interval, y = nSteps, group = Day)) + geom_point(size = 0.5, na.rm = TRUE) + facet_grid(~Day) + stat_smooth(method = gam, formula = y ~ s(x, bs = "cr"), na.rm = TRUE, family = quasipoisson) + ylab("Number of steps")
			print(p)
		}
		
	}	
	
	if(plot) {
		fit <- list("fit"=fit, "plot"=p)
	}	
	## Done!
	return(fit)
	
}
