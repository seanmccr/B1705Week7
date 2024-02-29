# ----- B1705 Week 7 | Advanced Time-Series Models and Approaches | 27.02.2024 -----
# ----- Pre - Lecture Work -----

# ----- 1. Example of ARIMA and SARIMA, same dataset: Practical -----

# Load necessary libraries
library(forecast)

# Setting seed and creating dataset
set.seed(123) # For reproducibility
seasonal_component <- sin(seq(1, 120, length.out = 120) * 2 * pi / 12) * 10
time_series_data <- ts(rnorm(120, mean = 10, sd = 5) + seasonal_component, frequency = 12)

# Plotting dataset
plot(time_series_data, main = "Time-Series with Seasonality", xlab = "Month", ylab = "Value")


# Decomposing the Time-Series
decomposed <- stl(time_series_data, s.window = "periodic")
plot(decomposed)

# ARIMA Model
arima_model <- auto.arima(time_series_data, seasonal = FALSE)
summary(arima_model)

# Forecast Plot
forecast_arima <- forecast(arima_model, h = 12)
plot(forecast_arima, main = "ARIMA Forecast")

# SARIMA Model
sarima_model <- auto.arima(time_series_data, seasonal = TRUE)
summary(sarima_model)

# Forecast Plot
forecast_sarima <- forecast(sarima_model, h = 12)
plot(forecast_sarima, main = "SARIMA Forecast")

# ----- Lecture Work -----
# ----- Panel Data: Demonstration -----
# ----- 1. Loading Packages, Libraries and Datasets -----

# Install and load the necessary package
install.packages("plm")
library(plm)
library(ggplot2)
library(broom)

# Creating Dataset
set.seed(123)
teams <- paste("Team", 1:6)
seasons <- 1:4

# Create data frame
num_teams <- length(teams)
num_seasons <- length(seasons)
observations <- num_teams * num_seasons

team_data <- expand.grid(team = teams, season = seasons)
team_data$goals <- rpois(observations, lambda = 30)
team_data$assists <- rpois(observations, lambda = 20)
team_data$fouls <- rpois(observations, lambda = 10)

# View dataset
head(team_data)

# ----- 2. Converting Dataframe -----

##### 2.1. Convert the data frame to a pdata.frame for panel analysis
panel_data <- pdata.frame(team_data, index = c("team", "season"))

##### 2.2. Exploratory DA: Plotting goals over seasons by team
ggplot(panel_data, aes(x = season, y = goals, group = team, color = team)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Total Goals Over 4 Seasons, by Team", x = "Season", y = "Goals")

##### 2.3. Create Fixed-Effect Model -----

# Fit a fixed effects model
fe_model <- plm(goals ~ assists + fouls, data = panel_data, model = "within")
summary(fe_model)

# Visualise the coefficients from the fixed effects model
coef_df <- tidy(fe_model)
ggplot(coef_df, aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  labs(title = "Fixed Effects Model Coefficients", x = "Predictors", y = "Estimates")

##### 2.4. Fit Random-Effects Model #####
# Fit a random effects model
re_model <- plm(goals ~ assists + fouls, data = panel_data, model = "random")
summary(re_model)

# Visualise coefficients from random effects model
re_coef_df <- tidy(re_model)
ggplot(re_coef_df, aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  labs(title = "Random Effects Model Coefficients", x = "Predictors", y = "Estimates")

# ----- 3. Further Examples -----

##### 3.1. Step 1 - Preparation #####
rm(list=ls())
# load packages
library(plm)

# Creating synthetic data
set.seed(123) # for reproducibility
players <- data.frame(
  player_id = rep(1:10, each=5),
  season = rep(2010:2014, times=10),
  goals = sample(0:30, 50, replace = TRUE),
  assists = sample(0:20, 50, replace = TRUE),
  minutes_played = sample(1800:3600, 50, replace = TRUE)
)

##### 3.2. Step 2 - Explore the Data #####
# Basic exploration
library(ggplot2)
library(dplyr)
library(plm)
library(ggplot2)
library(broom)

# Checking headings and summaries
head(players)
summary(players)

# Multiple Line Plot
ggplot(players, aes(x = season, y = goals, group = player_id, color = as.factor(player_id))) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Trend of Goals Scored Over Four Seasons, by Player",
       x = "Season",
       y = "Goals",
       color = "Player ID")

# Line plot
players %>%
  group_by(season) %>%
  summarize(average_assists = mean(assists)) %>%
  ggplot(aes(x = season, y = average_assists)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Average Assists Per Season",
       x = "Season",
       y = "Average Assists")

# Scatterplot
ggplot(players, aes(x = minutes_played, y = goals)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  theme_minimal() +
  labs(title = "Relationship Between Goals and Minutes Played",
       x = "Minutes Played",
       y = "Goals")

##### 3.3. Fit Panel Model #####

# Fit a fixed effects model
fe_model <- plm(goals ~ minutes_played + assists, data=players, 
                model="within", index=c("player_id", "season"))
summary(fe_model)

# Coefficient plot
coef_df <- tidy(fe_model)
ggplot(coef_df, aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  xlab("Variables") +
  ylab("Estimates") +
  ggtitle("Coefficient Plot for Fixed Effects Model")

# Time-Series Plot for specific player

player_data <- pdata.frame(players, index = c("player_id", "season"))
player_specific <- player_data[player_data$player_id == "2", ]
player_specific$fitted_values <- predict(fe_model, newdata = player_specific)

ggplot(player_specific, aes(x = season)) +
  geom_point(aes(y = fitted_values), color = "blue") +
  geom_point(aes(y = goals), color = "red") +  
  xlab("Season") +
  ylab("Fitted vs. Actual Goals") +
  ggtitle("Time-Series Plot of Fitted (blue) vs. Actual (red) Goals for Specific Player")


##### 3.4. Random-Effects Model #####

# Fit a random effects model
re_model <- plm(goals ~ minutes_played + assists, data=players, 
                model="random", index=c("player_id", "season"))
summary(re_model)

# Hausman test
phtest(fe_model, re_model)

# Here, p < 0.05 means we can reject the null hypothesis (selecting the random effects model as their are no
# significant differences between the RE and FE model estimes) because the Hausman test indicates that their IS a statistically significant
# difference between the estimates of the RE and FE models. If p > 0.05, we could fail to reject the null hypothesis, as the lack of statistically
# significant difference between RE and FE Model estimates means that, on the balance of probabilities, the RE model is more
# suitable as it's efficiency is better under the assumption of its consistency











