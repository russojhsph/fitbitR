#' Estimate the average number of steps taken per day
#'
#' Estimates the average number of steps taken per day naively (sample mean) or using an ARIMA model with the data binned by date.
#'
#' @param data A data set just like the one produced from \link{preprocess}.
#' @param method Must be \code{mean} or \code{auto.arima}.
#' @param acf If \code{TRUE} and \code{method} is set to \code{auto.arima} then an autocorrelation plot of the data binned by date is plotted.
#'
#' @details Check \link{reproduceAnalysis} with \code{step} set to \code{Q1} for more details on estimating the steps taken per day.
#'
#' @return A vector with the estimate, it's SE, and a 95% CI assuming using a t-dist. If \code{method} is set to \code{auto.arima} then it's a list where the first element is the vector previously described and the second element is the fitted object from the auto.arima model.
#'
#' @examples 
#' input <- fitbitData
#' data <- preprocess(input)
#' datap <- fitbitPred(data, method="lm")
#' q1(data, "mean")
#' q1(datap, "mean")
#' q1(data, "auto.arima")
#' q1(datap, "auto.arima")
#' @export
#' @seealso \link{binDay}

q1 <- function(data, method, acf=TRUE) {
	if(!method %in% c("mean", "auto.arima")) stop("'method' is incorrectly specified.")
	
	## Summarize the data by day
	datab <- binDay(data)
	## Drop NA's which can affect the df for the t-dist
	datab <- datab[complete.cases(datab), ]
	
	## Get the estimate and SD
	if(method == "mean") {
		estimate <- mean(datab$nSteps, na.rm=TRUE)
		est.se <- sd(datab$nSteps, na.rm=TRUE) / sqrt(nrow(datab))
	} else if (method == "auto.arima") {
		## Libs required
		require(xts)
		require(forecast)
		
		## Build the ts object
		datac <- datab[complete.cases(datab),]
		ts.c <- xts(datac$nSteps, order.by=datac$Date)
		
		## Autocorrelation plot
		if(acf) acf(ts.c, main="Autocorrelation plot for data binned by Date")
		
		## Fit the auto.arima model
		fit.a <- auto.arima(ts.c)
		
		## Get the estimate
		estimate <- coef(fit.a)["intercept"]
		names(estimate) <- NULL
		est.se <- sqrt(fit.a$var.coef["intercept", "intercept"])
	}
	
	## CI using a t-dist.
	l <- estimate - qt(0.975, df=nrow(datab)) * est.se
	u <- estimate + qt(0.975, df=nrow(datab)) * est.se
	
	## Build output
	res <- c("Estimate"=estimate, "SE"=est.se, "95% CI:L"=l, "95% CI:U"=u)
	
	## Add the model fit in case of auto.arima
	if(method == "auto.arima") {
		res <- list("Estimate"=res, "Fit"=fit.a)
	}
	
	## Done!
	return(res)
	
}

## Helper function that summarizes the data by day

#' Binned the data by date
#'
#' Bins the data by date.
#'
#' @param data A data set just like the one produced from \link{preprocess}.
#'
#'
#' @return A data.frame with 4 columns: Date, nSteps (the number of steps taken that day), Day: day of the week, and Weekend: whether it's a weekend or weekday.
#'
#' @examples 
#' input <- fitbitData
#' data <- preprocess(input)
#' datab <- binDay(data)
#' summary(datab)
#' @export
#' @seealso \link{q1}

binDay <- function(data) {
	nSteps <- tapply(data$nSteps, data$Date, sum)
	date <- unique(data$Date)
	day <- factor(weekdays(date), levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
	weekend <- factor(ifelse(day %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))
	
	## Result
	res <- data.frame(Date = date, nSteps = nSteps, Day = day, Weekend = weekend)
	rownames(res) <- 1:nrow(res)
	
	## Done
	return(res)
}
