#' Infer the average activity pattern by day
#'
#' Infers the average activity pattern over time (within a day).
#'
#' @param data A data set just like the one produced from \link{preprocess}.
#' @param method Must be \code{mean} or \code{gam}.
#' @param plot If \code{TRUE} the pattern is plotted.
#'
#' @details Check \link{reproduceAnalysis} with \code{step} set to \code{Q2} for more details on inferring the average activity pattern.
#'
#' @return A list with two elements: \code{Mean} with the average activity pattern and \code{SE} which is the SE.
#'
#' @examples 
#' input <- fitbitData
#' data <- preprocess(input)
#' q2.mean <- q2(data, "mean")
#' q2.gam <- q2(data, "gam")
#' @export

q2 <- function(data, method, plot=TRUE) {
	if(!method %in% c("mean", "gam")) stop("'method' is incorrectly specified.")
	
	## Drop NA's
	data <- data[complete.cases(data), ]
	
	## Get the estimate and SD
	if(method == "mean") {
		## Mean
		est <- tapply(data$nSteps, data$Interval, mean, na.rm=TRUE)
		## SE
		est.se <- tapply(data$nSteps, data$Interval, sd, na.rm=TRUE) / sqrt(table(data$Interval))
		est.se <- as.vector(est.se)
		
		if(plot) {
			grange <- range(c(est - est.se, est + est.se))
			plot(y=est, x=unique(data$IntS), type="l", col="blue", ylim=grange, axes=FALSE, ann=FALSE)
			
			## Annotation
			title(main = "Average activity pattern using mean method (with SE bands)")
			title(xlab = "Interval")
			title(ylab = "Number of steps")
			
			## SE bands
			lines(y=est - est.se, x=unique(data$IntS), lty=1, col="light blue")
			lines(y=est + est.se, x=unique(data$IntS), lty=1, col="light blue")
			
			## Axis
			axis(1, las=2, at=12*0:287, lab=unique(data$Interval)[1 + 12*0:287])
			axis(2, las=1, at=50*0:grange[2])

		}
		estimate <- list("Mean" = est, "SE" = est.se)
			
	} else if (method == "gam") {
		## Libs required
		require(mgcv)
		
		## Fit the model and then get the estimate for each interval along with the SE
		fit <- gam(nSteps ~ s(IntS, bs="cr"), data=data, family=quasipoisson)
		new <- data.frame(IntS = unique(data$IntS))
		estimate <- predict(fit, new, se.fit=TRUE, type="response")
		estimate <- lapply(estimate, function(x) { names(x) <- unique(data$Interval); x })
		names(estimate) <- c("Mean", "SE")
		
		if(plot) {
			## Plotting software
			require(ggplot2)
			
			## Make the plot
			p <- ggplot(data, aes(x=Interval, y=nSteps)) + geom_point(size=0.5, na.rm=TRUE) + stat_smooth(method=gam, formula= y ~ s(x, bs="cr"), na.rm=TRUE, family=quasipoisson) + ylab("Number of steps")
			print(p)
			estimate <- c(estimate, list("plot"=p))
		}
		
	}	
	
	## Done!
	return(estimate)
	
}
