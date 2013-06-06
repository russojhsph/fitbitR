## Setup
source("server.R")

## Specify layout
shinyUI(pageWithSidebar(
	
	headerPanel(HTML("Analyzing your <a href='http://www.fitbit.com/'>Fitbit</a> activity: number of steps in 5 min intervals"), "fitbitR"),
	
	
	sidebarPanel(
		## Construct input options
		
		## Choose the data
		h4("Load your data"),
		fileInput('file1', "CSV file. Check example data for format specifications.", accept=c('text/csv', 'text/comma-separated-values,text/plain')),
		tags$hr(),
		h4("Predict missing observations"),
		selectInput("pred", "Prediction method", choices = c("original", "lm", "poisson", "means", "overall-mean")),
		tags$hr(),
		HTML("For documentation and code check <a href='https://github.com/russojhsph/fitbitR'>fitbitR</a>.")
	),
		
	mainPanel(
		tabsetPanel(
			## Summary of the data. This is faster to load than the visualizations hence why I am showing this first..
			tabPanel("Data summary",
				h4("Data summary"),
				verbatimTextOutput("summary")
			),
			
			## Based on EDA results.
			tabPanel("Exploration",
				h4("Activity by date with weekday vs weekend comparison."),
				plotOutput("plot2"),
				h4("Activity by the clock for each day of the week and comparing weekdays vs weekends."),
				plotOutput("plot1")
			),
			
			## Questions 1 to 3 output using q1(), q2()	and q3()
			tabPanel("Q1",
				h4("Estimate the average number of steps taken per day"),
				h5("Estimate using the mean for data binned by day"),
				verbatimTextOutput("q1mean"),
				h5("Estimate using an ARIMA model for data binned by day"),
				plotOutput("q1acf"),
				verbatimTextOutput("q1aa")
			),
			
			tabPanel("Q2",
				h5("Will take a few seconds to load"),
				h4("Infer the average activity pattern by day"),
				h5("Average activity pattern: mean method"),
				plotOutput("q2mean"),
				h5("Average activity pattern: using a GAM model"),
				plotOutput("q2gam")
			),
			
			tabPanel("Q3",
				h5("Will take a few seconds to load"),
				h4("Infer differences between weekdays and weekends in patterns of activity"),
				h5("Weekdays vs Weekends"),
				plotOutput("q3weekend"),
				verbatimTextOutput("q3weekout"),
				h5("Pattern of activity by day of the week"),
				plotOutput("q3dow"),
				verbatimTextOutput("q3dowout")
			),
			
			tabPanel("Example data", 
				h4("Your data should be formatted like the following example."),
				verbatimTextOutput("head"),
				h5("..."),
				verbatimTextOutput("tail"),
				h5(HTML("Column 1 specifies the 5 minute intervals and the rest contain the actual number of steps. Column names (beyond column 1) specify the date. The total dimensions are 288 rows (+1 for the header) and X + 1 (row names) columns where X is the number of days recorded.")),
				h4("Download example data."),
				downloadButton('exampleData', 'Download')
			),
			
			tabPanel("Processed data", 
				h4("Download your processed data."),
				downloadButton('downloadData', 'Download')
			)		
			
		)
	)
	
))
