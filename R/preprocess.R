#' Pre-process the raw data
#'
#' Re-arranges the data for further analyses.
#'
#' @param input Must be a matrix such as the one in \link{fitbitData}.
#'
#' @return A data.frame with 7 columns: \code{Interval} specifying the 5 minute interval, \code{Date} specifying the date, \code{nSteps} which is the number of steps taken in that interval, the day of the week stored in \code{Day}, whether it's a weekend or weekday stored in \code{Weekend}, the actual time stamp in \code{Time}, and the interval transformed to a scale from 0 to 287 stored in \code{IntS}.
#'
#' @examples 
#' input <- fitbitData
#' data <- preprocess(input)
#' head(data)
#' summary(data)
#' @export
#' @seealso \link{fitbitData}

preprocess <- function(input) {
	## Check the input
	if(!is.matrix(input)) stop("'input' must be a matrix.")
	
	## Re-arrange the data into a data.frame with 3 columns: Interval, Date, and nSteps
	require(reshape2)
	data <- melt(input, varnames = c("Interval", "Date"), value.name = "nSteps")
	
	## Fix the dates
	data$Date <- as.Date(data$Date)
	
	## Weekday vs weekend
	data$Day <- factor(weekdays(data$Date), levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
	data$Weekend <- factor(ifelse(weekdays(data$Date) %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))
	
	## Time slot
	start <- data$Date[1]
	end <- data$Date[nrow(data)]
	data$Time <- seq(as.POSIXct(paste(start, "00:00:00"), tz = "UTC"), as.POSIXct(paste(end, "23:55:00"), tz = "UTC"), by = 60*5)
	
	## Transform the Interval to a scale in 0 to 287
	data$IntS <- 12 * (data$Interval %/% 100) + (data$Interval %% 100) / 5
		
	## Done =)
	return(data)
}
