# ----- B1705 Week 7 | Time Series Forecasting | 27.02.2024 -----

# ----- Decomposition and Moving Averages: Practical -----

# ----- 1. Loading Libraries and datasets -----

rm(list=ls())
library(forecast)
library(xts); # can do more than ts with 'xts'

df <- read.csv('https://www.dropbox.com/scl/fi/wb3t67831oe2j71u4znmk/tsa_forecast_02.csv?rlkey=0fzluocavie5eci0c7s6efstv&dl=1')
head(df)

# First, I'm going to tell R that one of my columns is a Date object
# I need to tell R the format the date is in
df$Date <- as.Date(df$Date, format = "%Y-%m-%d")

# Then, I create an xts object
swim_xts <- xts(df$Performance, order.by = df$Date)

# ----- 2. Plotting Original Time-Series -----

plot(swim_xts, main = "Daily Observations", ylab = "Av. Time(sec)", xlab = "Date")

# the `quantmod` package has some nice visual options
library(quantmod)
chart_Series(swim_xts)

# ----- 3. Decomposing the Time-Series ----- 

# Convert swim_xts to daily frequency using a custom summary, e.g., mean
swim_xts_weekly_mean <- apply.weekly(swim_xts, FUN = colMeans)

weekly_ts <- ts(swim_xts_weekly_mean, frequency=52)

# Decompose using an additive model (assumes seasonal variations are constant)
decomp_result_add <- decompose(weekly_ts, type="additive")

# Decompose using a multiplicative model (assumes seasonal variations change proprtionally)
decomp_result_mult <- decompose(weekly_ts, type="multiplicative")

# Plot results
plot(decomp_result_add)
plot(decomp_result_mult)


# -----  Classical Decomposition Practice -----

# Loading data
rm(decomp_result_add, decomp_result_mult, df, swim_xts, swim_xts_weekly_mean)
df <- read.csv('https://www.dropbox.com/scl/fi/kzfw4p1646azvsja6xrfr/tsa_forcast_01.csv?rlkey=zzyv9xoa36tvjclhnt0y6cggd&dl=1')

# First, extract only the attendance data
swim_data <- df$Attendance

# Then, convert it to a time series object:
swim_ts <- ts(swim_data, start=c(2010, 1), frequency=12)

plot(swim_ts, main="Monthly Swimming Pool Attendance", ylab="Attendance", xlab="Year")

# Decomposing the data 
decomposed_swim <- decompose(swim_ts)
plot(decomposed_swim)

# STL Approach
# Stock Market Example 

data("EuStockMarkets") # contains DAX index
eustock_ts <- ts(EuStockMarkets[, "DAX"], frequency=260)

# Plot this data
plot(eustock_ts, main="DAX Index", ylab="Index Value", xlab="Time")

# STL Decomposition; better for unpredictability/less seasonality within data 
stl_decomposed <- stl(eustock_ts, s.window="periodic")
plot(stl_decomposed)


# ----- 4. Moving Averages: Example -----

library(TTR)

df <- read.csv('https://www.dropbox.com/scl/fi/wb3t67831oe2j71u4znmk/tsa_forecast_02.csv?rlkey=0fzluocavie5eci0c7s6efstv&dl=1')
head(df)

data <- df$Performance

# Calculating the moving average 
sma_values <- SMA(data, n = 12)  # 12-period moving average

# Plot
plot(data, type = "l", col = rgb(0, 0, 0, 0.3), xlab = "Time", ylab = "Value", main = "Original Data and 12-Period SMA")
lines(sma_values, col = "blue", lwd = 2)  # Plot SMA values in blue and thicker

# Legend
legend("topright", legend = c("Original Data", "12-Period SMA"), col = c(rgb(0, 0, 0, 0.3), "blue"), lty = 1, lwd = c(1, 2))




# Calculating the exponential moving average (EMA)
ema_values <- EMA(data, n = 12)  # 12-period exponential moving average

# Plot
plot(data, type = "l", col = rgb(0, 0, 0, 0.3), xlab = "Time", ylab = "Value", main = "Original Data and 12-Period EMA")
lines(ema_values, col = "red", lwd = 2) 

# Legend
legend("topright", legend = c("Original Data", "12-Period EMA"), col = c(rgb(0, 0, 0, 0.3), "red"), lty = 1, lwd = c(1, 2))



# Plotting MA and EMA over each other 
plot(data, type = "l", col = rgb(0, 0, 0, 0.3), main = "MA and EMA Comparison", ylab = "Value")
lines(sma_values, col = "red")
lines(ema_values, col = "green")
legend("topright", legend = c("Original", "SMA", "EMA"), col = c(rgb(0, 0, 0, 0.3), "red", "green"), lty = 1)


# ----- 5. Shiny Object: Examples -----

library(shiny)
library(TTR)
library(ggplot2)

# Define UI
ui <- fluidPage(
  # Application title
  titlePanel("SMA and EMA on Time-Series Data"),
  
  # Sidebar layout with input controls for two moving averages
  sidebarLayout(
    sidebarPanel(
      # Input: Select type of the first moving average
      selectInput("maType1", "Select First Moving Average Type:",
                  choices = c("Simple Moving Average" = "SMA", 
                              "Exponential Moving Average" = "EMA")),
      # Input: Slider for the length of the first moving average
      sliderInput("maLength1", "Length of First Moving Average:",
                  min = 2, max = 50, value = 12),
      
      # Divider
      hr(),
      
      # Input: Select type of the second moving average
      selectInput("maType2", "Select Second Moving Average Type:",
                  choices = c("Simple Moving Average" = "SMA", 
                              "Exponential Moving Average" = "EMA")),
      # Input: Slider for the length of the second moving average
      sliderInput("maLength2", "Length of Second Moving Average:",
                  min = 2, max = 50, value = 26)
    ),
    
    # Show a plot of the generated time series with the selected moving averages
    
    
    mainPanel(
      plotOutput("timeSeriesPlot", width = "100%", height = "600px")
    )
    
  )
)

# Define server logic
server <- function(input, output) {
  
  # Generate a synthetic time series dataset
  syntheticData <- reactive({
    ts(rnorm(100, 20, 5), start = c(2020, 1), frequency = 12)
  })
  
  output$timeSeriesPlot <- renderPlot({
    data <- syntheticData()
    maType1 <- input$maType1
    maLength1 <- input$maLength1
    maType2 <- input$maType2
    maLength2 <- input$maLength2
    
    # Calculate the first moving average
    if (maType1 == "SMA") {
      maValues1 <- SMA(data, n = maLength1)
    } else if (maType1 == "EMA") {
      maValues1 <- EMA(data, n = maLength1)
    }
    
    # Calculate the second moving average
    if (maType2 == "SMA") {
      maValues2 <- SMA(data, n = maLength2)
    } else if (maType2 == "EMA") {
      maValues2 <- EMA(data, n = maLength2)
    }
    
    # Plot the original data and both moving averages
    plot(data, type = "l", col = "blue", main = "Time Series with Moving Averages", ylab = "Value", xlab = "Time")
    lines(maValues1, col = "red")
    lines(maValues2, col = "green")
    legend("topright", legend = c("Original Data", paste(maType1, maLength1), paste(maType2, maLength2)), col = c("blue", "red", "green"), lty = 1, cex = 0.8)
  })
}

# Run the application
shinyApp(ui = ui, server = server)


# Example 2

library(shiny)
library(tidyverse)
library(TTR) # For SMA and EMA

# Sample data frame
set.seed(123)
dates <- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by="day")
values <- rnorm(length(dates), mean = 100, sd = 10)
data <- data.frame(Date = dates, Value = values)

# Define UI
ui <- fluidPage(
  titlePanel("Dynamic Time Series Plot"),
  sidebarLayout(
    sidebarPanel(
      dateInput("startDate", "Start Date", value = min(data$Date), min = min(data$Date), max = max(data$Date)),
      dateInput("endDate", "End Date", value = max(data$Date), min = min(data$Date), max = max(data$Date)),
      numericInput("ma1", "Moving Average Period 1", value = 10, min = 1),
      numericInput("ma2", "Moving Average Period 2", value = 20, min = 1),
      selectInput("maType1", "MA Type 1", choices = c("SMA", "EMA")),
      selectInput("maType2", "MA Type 2", choices = c("SMA", "EMA"))
    ),
    mainPanel(
      plotOutput("timeSeriesPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$timeSeriesPlot <- renderPlot({
    # Filter data based on selected dates
    filteredData <- data %>%
      filter(Date >= input$startDate & Date <= input$endDate)
    
    # Calculate moving averages
    if(input$maType1 == "SMA") {
      filteredData$MA1 <- SMA(filteredData$Value, n = input$ma1)
    } else {
      filteredData$MA1 <- EMA(filteredData$Value, n = input$ma1)
    }
    
    if(input$maType2 == "SMA") {
      filteredData$MA2 <- SMA(filteredData$Value, n = input$ma2)
    } else {
      filteredData$MA2 <- EMA(filteredData$Value, n = input$ma2)
    }
    
    # Plot
    ggplot(filteredData, aes(x = Date)) +
      geom_line(aes(y = Value), colour = "blue") +
      geom_line(aes(y = MA1), colour = "red") +
      geom_line(aes(y = MA2), colour = "green") +
      labs(title = "Time Series with Moving Averages", x = "Date", y = "Value") +
      theme_minimal()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



