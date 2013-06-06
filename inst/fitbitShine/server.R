## Setup
library(fitbitR)
require(shiny)
require(ggplot2)

## Main shiny function
shinyServer(function(input, output) {
	
	## Read in the data
	dataInput <- reactive({
		inFile <- input$file1
		
		## Load the default data in the fitbitR package if the user hasn't uploaded his own
		if(is.null(inFile)) {
			data <- fitbitData
		} else {
			## Otherwise read the user supplied data
			data <- as.matrix(read.csv(inFile$datapath, row.names=1, check.names=FALSE))
		}		
		
		## Preprocess the data
		result <- preprocess(data)
	})
	
	## Choose between original or the prediction methods for the NA's
	predSet <- reactive({
		data <- dataInput()
		switch(input$pred,
			"original" = data,
			"lm" = fitbitPred(data, "lm"), 
			"poisson" = fitbitPred(data, "poisson"), 
			"means" = fitbitPred(data, "means"), 
			"overall-mean" = fitbitPred(data, "overall-mean")
		)
	})
	
	
	## Basic data frame summary
	output$summary <- renderPrint({
	    data <- predSet()
	
		## Basic summary
		summary(data)
		
	})
	
	
	
	## Polar plot with data grouped by Date and shown as points
	output$plot1 <- renderPlot({
		data <- predSet()
		p <- ggplot(data, aes(x=Interval, y=nSteps, group=Date, colour=Day, alpha=nSteps)) + geom_point(na.rm=TRUE)  + coord_polar() + facet_grid(~Weekend)
		print(p)
	})	
	
	## Data by Interval (time of day) and Date, also good for figuring out NA's and an overall pattern structure.
	output$plot2 <- renderPlot({
		data <- predSet()
		p <- ggplot(data, aes(y = Date, x = Interval, colour = nSteps)) + geom_point(na.rm=TRUE) + facet_grid(~Weekend)
		print(p)
	})	
	
	### Q1
	output$q1mean <- renderPrint({
		data <- predSet()
		q1(data, "mean")
	})	
	output$q1acf <- renderPlot({
		data <- predSet()
		q1.acf <- q1(data, "auto.arima")
	})	
	output$q1aa <- renderPrint({
		data <- predSet()
		q1(data, "auto.arima", acf=FALSE)
	})	
	
	### Q2
	output$q2mean <- renderPlot({
		data <- predSet()
		q2.mean <- q2(data, "mean")
	})	
	
	output$q2gam <- renderPlot({
		data <- predSet()
		q2.gam <- q2(data, "gam")
	})	
	
	### Q3
	output$q3weekend <- renderPlot({
		data <- predSet()
		q3.weekend <- q3(data, "weekend")
	})	
	output$q3dow <- renderPlot({
		data <- predSet()
		q3.dow <- q3(data, "dow")
	})	
	output$q3weekout <- renderPrint({
		data <- predSet()
		q3.weekend <- q3(data, "weekend", plot=FALSE)
		summary(q3.weekend)
	})	
	output$q3dowout <- renderPrint({
		data <- predSet()
		q3.dow <- q3(data, "dow", plot=FALSE)
		summary(q3.dow)
	})	
	
		
	
	## Show the top lines and first columns
	output$head <- renderPrint({
	   	head(fitbitData[, 1:6])
	})
	## Show the last lines and first columns
	output$tail <- renderPrint({
	   	tail(fitbitData[, 1:6])
	})

	## Download sample data in case the user needs to re-format his own data before uploading it
	output$exampleData <- downloadHandler(
	    filename  <-  function() { "fitbitData.csv" },
	    content  <-  function(file) {
	      write.csv(fitbitData, file)
	    }
	)
	
	## Download your processed data
	output$downloadData <- downloadHandler(
	    filename  <-  function() { paste("yourProcessedData", sample(1:10000, 1), '.csv', sep='') },
	    content  <-  function(file) {
			data <- predSet()
			write.csv(data, file)
	    }
	)
	
}) 