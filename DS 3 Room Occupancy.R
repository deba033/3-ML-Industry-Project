#x22242821
#
# packages and libraries
install.packages("ROCR")
install.packages("Metrics")
install.packages("lattice")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("nnet")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("plotly")
install.packages("gridExtra")
library(gridExtra)
library(plotly)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(caret)
library(randomForest)
library(tidyverse)
library(ROCR)
library(caret)
library(Metrics)
library(MASS)
library(nnet)
#
#
# Loading dataset
dataset <- read.csv("C:\\Users\\HP\\Desktop\\DMML PROJECT DATA\\Dataset 3 room+occupancy+estimation\\Occupancy_Estimation.csv")
#
#
# dataset summary
print(dataset)
head(dataset, 5)
summary(dataset)
str(dataset)
nrow(dataset)
#
#
# missing value check
missing_values <- colSums(is.na(dataset))
cat("Missing Values:\n")
print(missing_values)
#
#
# cleaning Time Feature
dataset_TS <- dataset
dataset_TS$DateTime <- paste(dataset_TS$Date, dataset_TS$Time, sep = ' ')
dataset_TS$DateTime <- dmy_hms(dataset_TS$DateTime)
head(dataset_TS, 5)
str(dataset_TS)
nrow(dataset_TS)
#
#
# Extract Weekday, Month and Hour
install.packages("dplyr")
library(dplyr)
dataset_TS$DateTime <- as.POSIXct(dataset_TS$DateTime, format = "%Y-%m-%d %H:%M:%S")
dataset_TS <- dataset_TS %>%
  mutate(Weekday = weekdays(DateTime))
dataset_TS <- dataset_TS %>%
  mutate(Month = months(DateTime))
library(lubridate)
dataset_TS <- dataset_TS %>%
  mutate(Hour = hour(DateTime))
head(dataset_TS, 5)
nrow(dataset_TS)
#
#
# checking null 
any(is.na(dataset))
#
#
# copying dataset for convenience 
dataset2<-dataset_TS
head(dataset2, 2)
nrow(dataset2)
#
#
# dropping Time column
install.packages("lubridate")
library(lubridate)
library(dplyr)
dataset2 <- dataset2 %>%
  select(-Time)
head(dataset2, 5)
str(dataset2)
nrow(dataset2)
#
#
# reordering the columns
str(dataset2)
dataset2 <- dataset2 %>%
  select(Date,Month,Weekday,DateTime,Hour,S1_Temp,S2_Temp,S3_Temp,S4_Temp,S1_Light,S2_Light,S3_Light,S4_Light,S1_Sound,S2_Sound,S3_Sound,S4_Sound,S5_CO2,S5_CO2_Slope,S6_PIR,S7_PIR,Room_Occupancy_Count)
head(dataset2, 2)
nrow(dataset2)
#
#
# Boxplot before Outlier removal
num_col <- sapply(dataset2, is.numeric)
num_data <- dataset2[, num_col]
num_data <- num_data[, !colnames(num_data) %in% 'Room_Occupancy_Count']
head(num_data, 5)
boxplt <- list()
library(ggplot2)
for (col in colnames(num_data)) {
  boxplot <- ggplot(num_data, aes(y = .data[[col]])) +
    geom_boxplot() +
    labs(title = paste("Boxplot of ", col), y = col) +
    theme_classic()
  boxplt[[col]] <- boxplot
}
library(gridExtra)
subplot_list <- grid.arrange(grobs = boxplt, ncol = 4)
subplot_list 
#
#
# Outlier percentage check
num_col <- sapply(dataset2, is.numeric)
num_data <- dataset2[, num_col]

outlier_percentage <- function(column) {
  q <- quantile(column, c(0.25, 0.75))
  iqr <- q[2] - q[1]
  lower_bound <- q[1] - 1.5 * iqr
  upper_bound <- q[2] + 1.5 * iqr
  
  outliers <- column < lower_bound | column > upper_bound
  percentage <- sum(outliers) / length(column) * 100
  return(percentage)
}
outlier_percentages <- sapply(num_data, outlier_percentage)
cat("Outlier Percentages:\n")
for (i in seq_along(outlier_percentages)) {
  cat(names(outlier_percentages)[i], ": ", outlier_percentages[i], "%\n")
}
#
#
# Function to filter rows based on z-score
print(nrow(dataset2))
remove_outliers <- function(dataset2, threshold = 2) {
  numeric_data <- dataset2[sapply(dataset2, is.numeric)]
  z_scores <- scale(numeric_data)
  outliers <- which(abs(z_scores) > threshold, arr.ind = TRUE)
  data_no_outliers <- dataset2[-outliers[, 1], ]
  return(data_no_outliers)
}
dataset2_no_outliers <- remove_outliers(dataset2)
str(dataset2_no_outliers)
print(nrow(dataset2_no_outliers))
dataset_filtered<-dataset2_no_outliers
unique_counts <- table(dataset2_no_outliers$Room_Occupancy_Count)
print(unique_counts)
install.packages("writexl")
library(writexl)
file_path <- "C:\\Users\\HP\\Desktop\\file_save.xlsx"
write_xlsx(dataset_filtered, file_path, col_names = TRUE)
#
#
# Outlier percentage after outlier removal
num_col <- sapply(dataset_filtered, is.numeric)
num_data <- dataset_filtered[, num_col]
outlier_percentage <- function(column) {
  q <- quantile(column, c(0.25, 0.75))
  iqr <- q[2] - q[1]
  lower_bound <- q[1] - 1.5 * iqr
  upper_bound <- q[2] + 1.5 * iqr
  
  outliers <- column < lower_bound | column > upper_bound
  percentage <- sum(outliers) / length(column) * 100
  return(percentage)
}
outlier_percentages <- sapply(num_data, outlier_percentage)
cat("Outlier Percentages after:\n")
for (i in seq_along(outlier_percentages)) {
  cat(names(outlier_percentages)[i], ": ", outlier_percentages[i], "%\n")
}
#
#
# boxplots after outlier removal
num_col2 <- sapply(dataset_filtered, is.numeric)
num_data2 <- dataset_filtered[, num_col2]
boxplt <- list()
for (col in colnames(num_data2)) {
  boxplot <- ggplot(num_data2, aes(y = .data[[col]])) +
    geom_boxplot() +
    labs(title = paste("Boxplot of ", col), y = col) +
    theme_minimal()  
  boxplt[[col]] <- boxplot
}
subplot_list <- grid.arrange(grobs = boxplt, ncol = 4)
subplot_list
print(nrow(dataset_filtered))
str(dataset_filtered)
#
# 
# Visualization
# Distribution of Room_Occupancy_Count
library(ggplot2)
ggplot(dataset2, aes(x = Room_Occupancy_Count)) +
  geom_bar(fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Room_Occupancy_Count")
#
#
# Correlation matrix 
any(is.na(dataset_filtered))
install.packages("dplyr")
library(dplyr)
dataset_new <- dataset_filtered %>% filter(!any(is.na(.)))
print(dataset_new)
numeric_cols <- sapply(dataset_filtered, is.numeric)
num_data <- dataset_filtered[, numeric_cols]
non_constant_cols <- sapply(num_data, sd) != 0
num_data <- num_data[, non_constant_cols]
correlation_matrix <- cor(num_data)
library(reshape2)
library(ggplot2)
melted_corr <- melt(correlation_matrix)
corr_plot <- ggplot(data = melted_corr, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), vjust = 1) +  
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1),
        axis.text.y = element_text(size = 8)) +
  labs(x = "Variable 1", y = "Variable 2")
print(corr_plot)
#
#
# data scaling by filtering Highest correlated column 
library(caret)
#numeric_cols_2 <- sapply(dataset_filtered, is.numeric)
numeric_cols_2 <- sapply(dataset_filtered[, !names(dataset_filtered) %in% c("Room_Occupancy_Count")], is.numeric)

num_data_2 <- dataset_filtered[, numeric_cols_2]
non_constant_cols_2 <- sapply(num_data_2, sd) != 0
num_data_2 <- num_data_2[, non_constant_cols_2]
cor_matrix <- cor(num_data_2)
#cor_matrix <- cor(dataset_filtered[, numeric_cols])
high_cor_features <- findCorrelation(cor_matrix, cutoff = 0.8)
dataset_filtered2 <- dataset_filtered[, -high_cor_features]
print(head(dataset_filtered2))
print(nrow(dataset_filtered2))
#
#
# Correlation matrix after scaling
numeric_cols2 <- sapply(dataset_filtered2, is.numeric)
num_data2 <- dataset_filtered2[, numeric_cols2]
non_constant_cols2 <- sapply(num_data2, sd) != 0
num_data2 <- num_data2[, non_constant_cols2]
correlation_matrix2 <- cor(num_data2)
#correlation_matrix2 <- cor(dataset_filtered[, c("S1_Temp", "S2_Temp", "S3_Temp", "S4_Temp", "S1_Light", "S2_Light", "S3_Light", "S4_Light", "S1_Sound", "S2_Sound", "S3_Sound", "S4_Sound", "S5_CO2", "S5_CO2_Slope", "S6_PIR", "S7_PIR")])
library(reshape2)
library(ggplot2)
melted_corr2 <- melt(correlation_matrix2)
corr_plot2 <- ggplot(data = melted_corr2, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), vjust = 1) +  
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1),
        axis.text.y = element_text(size = 8)) +
  labs(x = "Variable 1", y = "Variable 2")
print(corr_plot2)
#
#
#-----------------------------------------------------------------------
# Log transform
# log transformation (but didn't provided good result, so ignored)
num_data_log_transformed <- log1p(num_data)
dataset_filtered[, numeric_cols] <- num_data_log_transformed
correlation_matrix_after_log <- cor(dataset_filtered[, numeric_cols])
print(correlation_matrix_after_log)
#-----------------------------------------------------------------------
#
#
# Modeling
#---------------------------------------------------
# Train-test split
install.packages("caret")
library(caret)
set.seed(22242821)
train_index <- createDataPartition(dataset2$Room_Occupancy_Count, p = 0.8, list = FALSE)
train_data <- dataset2[train_index, ]
test_data <- dataset2[-train_index, ]
#
#
# Logistic Regression Model
# Set the threshold value
threshold_value <- 0.5
#
#
# converting Room_Occupancy_Count to binary
train_data$Room_Occupancy_Count_binary <- ifelse(train_data$Room_Occupancy_Count > threshold_value, 1, 0)
test_data$Room_Occupancy_Count_binary <- ifelse(test_data$Room_Occupancy_Count > threshold_value, 1, 0)
#
#
# selecting features
features <- c("S1_Temp", "S2_Temp", "S3_Temp", "S4_Temp", "S1_Light", "S2_Light", "S3_Light", "S4_Light", "S1_Sound", "S2_Sound", "S3_Sound", "S4_Sound", "S5_CO2", "S5_CO2_Slope", "S6_PIR", "S7_PIR")
target_binary <- "Room_Occupancy_Count_binary"
#
#
# training the logistic regression model
logistic_model <- glm(formula = as.formula(paste(target_binary, "~", paste(features, collapse = "+"))), data = train_data, family = "binomial")
#
#
# predicting probabilities on the test set
probabilities <- predict(logistic_model, newdata = test_data, type = "response")
#
#
# converting probabilities to binary predictions
predictions <- ifelse(probabilities > threshold_value, 1, 0)
#
#
# Evaluation
# confusion matrix
conf_matrix <- table(predictions, test_data$Room_Occupancy_Count_binary)
print(conf_matrix)
#
#
# calculate Accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy is:", accuracy, "\n")
print(accuracy)
#
#
# calculating classification metrics like precision, recall, F1-score
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("Precision is:", precision, "\n")
cat("Recall value is:", recall, "\n")
cat("F1 Score is:", f1_score, "\n")


