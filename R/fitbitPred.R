#' Predict values for the missing data.
#'
#' Predicts the number of steps for the missing intervals.
#'
#' @param data A data set just like the one produced from \link{preprocess}.
#' @param method Must be \code{poisson}, \code{lm}, \code{means} or \code{overall-mean}.
#'
#' @details Check \link{reproduceAnalysis} with \code{step} set to \code{pred} for more details on the exploration of the prediction methods. Four of them were chosen from there and are implemented in \code{fitbitPred} although you should be warned that the root mean squared prediction error is around 100.
#'
#' @return A data.frame where missing observations have been replaced with the predicted values.
#'
#' @examples 
#' input <- fitbitData
#' data <- preprocess(input)
#' head(data)
#' summary(data)
#' new <- fitbitPred(data, method="lm")
#' summary(new)
#' @export
#' @seealso \link{fitbitData}, \link{preprocess}


fitbitPred <- function(data, method) {
	if(!method %in% c("poisson", "lm", "means", "overall-mean")) stop("'method' is not specified correctly")
	
	## Separate missing vs complete data
	idx <- complete.cases(data)
	datac <- data[idx,]
	datana <- data[!idx,]	
	
	if(method == "means") {
		require(plyr)
		## Get the means from the observed data grouped by Interval & Day
		means <- ddply(datac, .(Interval, Day), summarize, meanNsteps = mean(nSteps))
		
		## Helper function that basically subsets the means summarized data and finds the one with which to replace the missing observation.
		findMean <- function(int, d, dat=means) {
			res <- subset(dat, Interval==int & Day==d)$meanNsteps
			return(res)
		}
		
		## Get the predictions using the means from similar cases
		pred <- apply(datana[, c("Interval", "Day")], 1, function(x) { findMean(as.integer(x[1]), x[2])})
		
	} else if (method == "lm") {
		require(splines)
		## Fit a linear model using natural splines for the interval
		fit.lm <- lm(nSteps ~ ns(Interval, 10) + Date + Day, data=datac)
		
		## Predict for the missing data
		pred <- predict(fit.lm, datana)
		
		## Truncate if below 0
		pred[pred < 0] <- 0
		
	} else if (method == "poisson") {
		require(splines)
		## Fit a GLM with a Poisson link and natural splines
		fit.glm <- glm(nSteps ~ ns(Interval, 10) + Date + Day, data=datac, family=poisson)
		
		## Predict for the missing data
		pred <- predict(fit.glm, datana, type="response")
	} else if (method == "overall-mean") {
		pred <- rep(mean(datac$nSteps), nrow(datana))
	}
	
	## Exchange the NA's with the predicted data
	data$nSteps[!idx] <- pred
	
	## Done!
	return(data)
}
