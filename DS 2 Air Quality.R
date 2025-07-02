#x22242821
#
#
#reading the file which is in csv format
aq_data <- read.csv("C:\\Users\\HP\\Desktop\\DMML PROJECT DATA\\AirQualityUCI_TS 1.csv", header=T, na.strings=c(""), stringsAsFactors = T)
summary(aq_data)
str(aq_data)
#
#
#working on time feature
#cleaning and expanding time data
install.packages("lubridate")
install.packages("dplyr")
library(lubridate)
library(dplyr)
aq_data$DateTime <- paste(aq_data$Date, aq_data$Time, sep = ' ')
aq_data$DateTime <- mdy_hms(aq_data$DateTime)
aq_data$DateTime <- as.POSIXct(aq_data$DateTime, origin = "1970-01-01", tz = "UTC")
print(aq_data)
aq_data <- aq_data %>%
  mutate(Weekday = weekdays(DateTime))
aq_data <- aq_data %>%
  mutate(Month = months(DateTime))
aq_data <- aq_data %>%
  mutate(Hour = hour(DateTime))
aq_data <- aq_data %>%
  mutate(Year = year(DateTime))
#
#
#null handling
aq_data <- subset(aq_data, select = -c(Unnamed..15, Unnamed..16))
str(aq_data)
#Substitute faulty sensor measurements marked with a value of -200 with NaN
aq_data <- aq_data%>%
mutate_all(~ifelse(. == -200, NA, .))
#null checking
any(is.na(aq_data))
#null percentage check
install.packages("dplyr")
install.packages("tidyr")
library(dplyr)
library(tidyr)
nan_perc <- aq_data %>%
  summarise_all(~(sum(is.na(.)) / n()) * 100) %>%
  pivot_longer(everything(), names_to = "Column", values_to = "%_NaN_in_Column") %>%
  arrange(desc("%_NaN_in_Column"))
print(nan_perc)
#remove column with 90.2% NAN: NMHC.GT
aq_data_new <- aq_data %>% select(-NMHC.GT.)
str(aq_data_new)
#na row removing
aq_data_new <- na.omit(aq_data_new)
str(aq_data_new)
print(head(aq_data_new))
#dropping time column from dataframe and reordering
aq_data_new <- aq_data_new %>%
  select(-Time)
aq_data_new <- aq_data_new %>%
  select(Date, Year, Month, Weekday, DateTime, Hour, `CO.GT.`, `PT08.S1.CO.`, `C6H6.GT.`, `PT08.S2.NMHC.`, `NOx.GT.`, `PT08.S3.NOx.`, `NO2.GT.`, `PT08.S4.NO2.`, `PT08.S5.O3.`, T, RH, AH)
#
#
#Outlier handling
#boxplot for numeric columns
install.packages("plotly")
install.packages("ggplot2")
install.packages("gridExtra")
library(ggplot2)
library(gridExtra)
library(plotly)
numeric_cols <- sapply(aq_data_new, is.numeric) & !colnames(aq_data_new) %in% c('Date', 'DateTime','Year')
numeric_dt <- aq_data_new[, numeric_cols]
print(numeric_dt)
custom_colors <- c( "#87CEEB")
boxplots <- list()
for (i in seq_along(colnames(numeric_dt))) {
  col <- colnames(numeric_dt)[i]
  color <- custom_colors
  boxplot <- ggplot(aq_data_new, aes(y = .data[[col]], fill = factor(1))) +
    geom_boxplot() +
    scale_fill_manual(values = color) +  
    labs(title = paste("Boxplot of Numeric Column:", col), y = col) +
    theme_minimal()
  boxplots[[col]] <- boxplot
}
num_cols <- 4
num_rows <- ceiling(length(boxplots) / num_cols)
grid.arrange(grobs = boxplots, ncol = num_cols)
#outlier removal using IQR method
library(dplyr)
numeric_cols <- aq_data_new %>% select_if(is.numeric)
scale <- 1.4
filter_outliers <- function(col) {
  Q <- quantile(col, c(0.25, 0.75))
  IQR_val <- IQR(col)
  lower_lim <- Q[1] - scale * IQR_val
  upper_lim <- Q[2] + scale * IQR_val
  col_filtered <- col >= lower_lim & col <= upper_lim
  return(col_filtered)
}
conditions <- lapply(numeric_cols, filter_outliers)
final <- Reduce(`&`, conditions)
aq_data_f2 <- aq_data_new[final, ]
print(head(aq_data_f2))
#copying for convenience
aq_filtered<-aq_data_f2
#boxplot for outlier checking after removal
install.packages("plotly")
install.packages("ggplot2")
install.packages("gridExtra")
library(ggplot2)
library(gridExtra)
library(plotly)
n_cols <- sapply(aq_filtered, is.numeric) & !colnames(aq_filtered) %in% c('Date', 'DateTime','Year')
num_data <- aq_filtered[, n_cols]
print(num_data)
custom_colors <- c( "#87CEEB")
boxplots_no_out <- list()
for (i in seq_along(colnames(num_data))) {
  col <- colnames(num_data)[i]
  color <- custom_colors
  boxplot <- ggplot(aq_filtered, aes(y = .data[[col]], fill = factor(1))) +
    geom_boxplot() +
    scale_fill_manual(values = color) +  
    labs(title = paste("Boxplot after outlier removal", col), y = col) +
    theme_minimal()
  boxplots[[col]] <- boxplot
}
num_cols <- 4
num_rows <- ceiling(length(boxplots) / num_cols)
grid.arrange(grobs = boxplots, ncol = num_cols)
#
#
#visualizations
#scatter plot
num_cols <- ncol(num_data)
num_rows <- (num_cols - 1) %/% 3 + 1
plots <- list()
for (i in seq_along(names(num_data))) {
  column <- names(num_data)[i]
  plot <- ggplot(num_data, aes(x = .data[[column]], y = CO.GT.)) +
    geom_point(color = "#87CEEB") +  
    labs(title = paste(column, "vs. CO.GT."), x = column, y = "CO.GT.") +
    theme_minimal()
  plots[[i]] <- plot
}
subplot_list <- patchwork::wrap_plots(plots, ncol = 3)
subplot_list + ggplot2::ggtitle("Scatter Plots for Air Quality Data") +
  theme(plot.title = element_text(hjust = 0.5))
#Line plot on dependent column with date
library(lubridate)
library(ggplot2)
ggplot(aq_filtered, aes(x = Date, y = CO.GT.)) +
  geom_line() +
  labs(title = "Time Series Plot", x = "Date", y = "CO.GT.")
# Lag plot on dependent column with date
ggplot(aq_filtered, aes(x = lag(x = Date), y = (CO.GT.))) +
  geom_point() +
  labs(title = "Lag Plot", x = "Date", y = "CO.GT.")
#
#
#Correlation matrix
install.packages("corrplot")
library(corrplot)
library(gridExtra)
correlation_matrix <- cor(num_data)
par(mfrow = c(1, 1))
corrplot(
  correlation_matrix,
  method = "color",
  col = colorRampPalette(c("green", "white", "yellow"))(50),
  addCoef.col = "black",
  is.corr = FALSE,
  width = 20,  
  height = 20 
)
title("Heatmap for Correlation Matrix")
#
#Line plots:
#Monthly line plot for CO.GT.
library(ggplot2)
ggplot(aq_filtered, aes(x = Month, y = CO.GT.)) +
  geom_line(color = "#0000FF") +
  labs(title = "Monthly Time Series of CO.GT.",
       x = "Month",
       y = "Mean CO.GT.") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Yearly line plot CO.GT. 
ggplot(aq_filtered, aes(x = Year, y = CO.GT.)) +
  geom_line(color = "#0000FF") +
  labs(title = "Yearly Time Series of CO.GT.",
       x = "Year",
       y = "Mean CO.GT.") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Weekly line plot CO.GT. 
ggplot(aq_filtered, aes(x = Weekday, y = CO.GT.)) +
  geom_line(color = "#0000FF") +
  labs(title = "Weekly Time Series of CO.GT.",
       x = "Weekday",
       y = "Mean CO.GT.") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Hourly line plot CO.GT. 
ggplot(aq_filtered, aes(x = Hour, y = CO.GT.)) +
  geom_line(color = "#0000FF") +
  labs(title = "Hourly Time Series of CO.GT.",
       x = "Hour",
       y = "Mean CO.GT.") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#
#barplot:
#Hourly Bar plot CO.GT.
library(ggplot2)
ggplot(aq_filtered, aes(x = Hour, y = CO.GT.)) +
  geom_bar(stat = "summary", fun = "mean", fill = "skyblue") +
  labs(title = "Hourly Time Series of CO.GT.",
       x = "Hour",
       y = "Mean CO.GT.") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Weekly Bar plot CO.GT. 
ggplot(aq_filtered, aes(x = Weekday, y = CO.GT.)) +
  geom_bar(stat = "summary", fun = "mean", fill = "skyblue") +
  labs(title = "Weekly Time Series of CO.GT.",
       x = "Weekday",
       y = "Mean CO.GT.") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Monthly Bar plot CO.GT. 
ggplot(aq_filtered, aes(x = Month, y = CO.GT.)) +
  geom_bar(stat = "summary", fun = "mean", fill = "skyblue") +
  labs(title = "Monthly Time Series of CO.GT.",
       x = "Month",
       y = "Mean CO.GT.") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  
#Yearly bar plot CO.GT. 
ggplot(aq_filtered, aes(x = Year, y = CO.GT.)) +
  geom_bar(stat = "summary", fun = "mean", fill = "skyblue") +
  labs(title = "Yearly Time Series of CO.GT.",
       x = "Year",
       y = "CO.GT.") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#
#
#trend and decomposition check 
install.packages("lubridate")
install.packages(c("forecast", "ggplot2"))
library(forecast)
library(ggplot2)
library(lubridate)
#creating date index
aq_filtered$DateTime <- as.POSIXct(aq_filtered$DateTime, origin = "1970-01-01", tz = "UTC")
aq_filtered$Year <- year(aq_filtered$DateTime)
str(aq_filtered)
ts_data_exp <- ts(aq_filtered$CO.GT., frequency = 365, start = c(year(aq_filtered$DateTime[1]), month(aq_filtered$DateTime[1])))
#time series decomposition to plot the original time series, trend, and residuals
decomp <- ma(ts_data_exp, order = 12)
autoplot(decomp)
#
#
#Modeling Time series Data
# EXPONENTIAL SMOOTHING
library(zoo)
library(forecast)
library(ggplot2)
aq_filtered$Date <- as.POSIXct(aq_filtered$Date)
total_length <- nrow(aq_filtered)
train_prop <- 0.8
train_length <- round(train_prop * total_length)
train_data <- aq_filtered[1:train_length, ]
test_data <- aq_filtered[(train_length + 1):total_length, ]
par(mfrow = c(1, 1))
acf(train_data$CO.GT., lag.max = 48, main = "ACF (Training Set)")
pacf(train_data$CO.GT., lag.max = 48, main = "PACF (Training Set)")
par(mfrow = c(1, 1))
print(train_data$CO.GT.)
ets_model <- ets(train_data$CO.GT.)
ets_forecast <- forecast(ets_model, h = nrow(test_data))
print(ets_forecast)
combined_data <- data.frame(
  Date = c(train_data$Date, test_data$Date),
  Original = c(train_data$CO.GT., rep(NA, nrow(test_data))),
  Forecast = c(rep(NA, nrow(train_data)), ets_forecast$mean)
)
print(tail(combined_data, 30))
autoplot(ets_forecast, series = "Original") +
  autolayer(ets_forecast, series = "Forecast", linetype = "dashed") +
  labs(title = "Exponential Smoothing using ETS for Air Quality", y = "CO.GT.") +
  scale_color_manual(values = c("Original" = "black", "Forecast" = "blue")) +
  theme_classic()
rmse <- sqrt(mean((combined_data$Forecast[nrow(train_data) + 1:nrow(test_data)] - test_data$CO.GT.)^2))
mae <- mean(abs(combined_data$Forecast[nrow(train_data) + 1:nrow(test_data)] - test_data$CO.GT.))
mape <- mean(abs((combined_data$Forecast[nrow(train_data) + 1:nrow(test_data)] - test_data$CO.GT.) / test_data$CO.GT.)) * 100
cat("Root Mean Sqaure Error:", rmse, "\n")
cat("Mean Absolute Error:", mae, "\n")
cat("Mean Absolute Percentage Error:", mape, "%\n")
#
#
#SARIMA Model
library(zoo)
library(forecast)
library(ggplot2)
aq_filtered$Date <- as.POSIXct(aq_filtered$Date)
total_length <- nrow(aq_filtered)
train_prop <- 0.8
train_length <- round(train_prop * total_length)
train_data <- aq_filtered[1:train_length, ]
test_data <- aq_filtered[(train_length + 1):total_length, ]
par(mfrow = c(1, 1))
acf(train_data$CO.GT., lag.max = 48, main = "ACF (Training Set)")
pacf(train_data$CO.GT., lag.max = 48, main = "PACF (Training Set)")
par(mfrow = c(1, 1))
sarima_model <- Arima(train_data$CO.GT., order = c(3, 1, 2), seasonal = list(order = c(1, 1, 1), period = 12))
sarima_forecast <- forecast(sarima_model, h = nrow(test_data))
combined_data <- data.frame(
  Date = c(train_data$Date, test_data$Date),
  Original = c(train_data$CO.GT., rep(NA, nrow(test_data))),
  Forecast = c(rep(NA, nrow(train_data)), sarima_forecast$mean)
)
print(tail(combined_data, 30))
autoplot(sarima_forecast, series = "Original") +
  autolayer(sarima_forecast, series = "Forecast", linetype = "dashed") +
  labs(title = "SARIMA Forecast for Air Quality", y = "CO.GT.") +
  scale_color_manual(values = c("Original" = "red", "Forecast" = "blue")) +
  theme_classic()
rmse <- sqrt(mean((combined_data$Forecast[nrow(train_data) + 1:nrow(test_data)] - test_data$CO.GT.)^2))
mae <- mean(abs(combined_data$Forecast[nrow(train_data) + 1:nrow(test_data)] - test_data$CO.GT.))
mape <- mean(abs((combined_data$Forecast[nrow(train_data) + 1:nrow(test_data)] - test_data$CO.GT.) / test_data$CO.GT.)) * 100
cat("Root Mean Sqaure Error:", rmse, "\n")
cat("Mean Absolute Error:", mae, "\n")
cat("Mean Absolute Percentage Error:", mape, "%\n")
summary_sarima <- summary(sarima_model)
print(summary_sarima)
#
#
#-----------------------------------------------------------------------#

