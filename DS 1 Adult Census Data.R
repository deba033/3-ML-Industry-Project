##x22242821
#
# libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(randomForest)
library(xgboost)
#
#
# loading Adult dataset
# (dataset has no header present)
adult_data <- read.csv("C:\\Users\\HP\\Desktop\\DMML PROJECT DATA\\Dataset 1 adult\\adult.csv", header = FALSE, na.strings = c("?", " ?"))
#
#
# Define column names
# (adding header name as per source data)
colnames(adult_data) <- c("age", "workclass", "fnlwgt", "education", "education_num", "marital_status",
                          "occupation", "relationship", "race", "sex", "capital_gain", "capital_loss",
                          "hours_per_week", "native_country", "income")
#
#
# exploratory analysis of data
# data summary
str(adult_data)
summary(adult_data)
View(adult_data)
#
#
# missing value check
# handling missing values
adult_data[adult_data == "?"] <- NA
missing_values <- colSums(is.na(adult_data))
print(missing_values)
print(head(adult_data))
adult_data <- subset(adult_data, !is.na(native_country))
print(head(adult_data))
print(colSums(is.na(adult_data)))
print(nrow(adult_data))

#
#
# replacing missing values in two columns with MODE
m_occ <- names(sort(table(adult_data$occupation), decreasing = TRUE)[1])
adult_data$occupation[is.na(adult_data$occupation)] <- m_occ
m_wrc <- names(sort(table(adult_data$workclass), decreasing = TRUE)[1])
adult_data$workclass[is.na(adult_data$workclass)] <- m_wrc
print(nrow(adult_data))
#
#
# create a table for education vs income
windows(width = 30, height = 20)
adult_data$income <- as.factor(adult_data$income)
adult_data$education <- as.factor(adult_data$education)
edu_inc <- table(adult_data$education, adult_data$income)
barplot(edu_inc, beside = TRUE, col = c("#3486db", "#f44e59", "#7f8c8d", "#c39d01", "#9b59b6", "#34495e",
                                                    "#1abc9c", "#e67e22", "#95a5a6", "#d35400", "#bdc3c7", "#1ca085",
                                                    "#c0392b", "#3f3c8d", "#27ae60", "#1674ed"), legend.text = TRUE,
        args.legend = list(x = "topright", bty = "n", legend = c(" ", " ")),
        main = "Education vs Income", xlab = "Education Level", ylab = "Count")
legend("topright", legend = levels(adult_data$education), fill = c("#3486db", "#f44e59", "#7f8c8d", "#c39d01", "#9b59b6", "#34495e",
                                                                   "#1abc9c", "#e67e22", "#95a5a6", "#d35400", "#bdc3c7", "#1ca085",
                                                                   "#c0392b", "#3f3c8d", "#27ae60", "#1674ed"), cex = 1)
#
#
# convert categorical columns to factors using loop
categorical_cols <- c("workclass", "education", "marital_status", "occupation", "relationship", "race", "sex", "native_country", "income")
for (col in categorical_cols) {
  levels <- unique(adult_data[[col]])
  adult_data[[col]] <- as.integer(factor(adult_data[[col]], levels = levels))
}
head(adult_data)
#
#
# correlation check
numeric_cols <- c("age", "fnlwgt", "education_num", "capital_gain", "capital_loss", "hours_per_week")
cor_matrix <- cor(adult_data[, numeric_cols])
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", tl.col = "blue", tl.srt = 45, addCoef.col = "black", number.cex = 0.7, col = colorRampPalette(c("#3498db", "#ffffff", "#e74c3c"))(100))
#
#
# boxplot for numeric columns before outlier removal
numeric_cols <- c("age", "fnlwgt", "education_num", "capital_gain", "capital_loss", "hours_per_week")
adult_data_long <- adult_data %>%
  select(all_of(numeric_cols)) %>%
  gather(key = "Variable", value = "Value")
ggplot(adult_data_long, aes(x = Variable, y = Value)) +
  geom_boxplot(fill = "#1299db") +
  labs(title = "Boxplot", x = "Features", y = "Values") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(nrow(adult_data))
#
#
# outlier Removal
# (removing capital loss and capital gain outliers)
z_scores <- scale(adult_data$capital_gain)
print(z_scores)
outliers <- which(abs(z_scores) > 2)
adult_data_new <- adult_data[-outliers, ]
z_scores <- scale(adult_data$capital_loss)
outliers <- which(abs(z_scores) > 2)
adult_data_new <- adult_data_new[-outliers, ]
#
#
# handling outliers and normalizing data
print(nrow(adult_data))
cols_to_normalize <- c("capital_gain", "capital_loss", "education_num")
for (col in cols_to_normalize) {
  z_scores <- scale(adult_data[[col]])
  outliers <- which(abs(z_scores) > 2)
  cleaned_df <- adult_data[-outliers, ]
}
normalize_minmax <- function(column) {
  (column - min(column, na.rm = TRUE)) / (max(column, na.rm = TRUE) - min(column, na.rm = TRUE))
}
new_df <- as.data.frame(lapply(cleaned_df, normalize_minmax))
print(head(new_df))
print(nrow(new_df))
#
#
# boxplot for all numeric columns after outlier handling
numeric_cols <- c("age", "fnlwgt", "education_num", "capital_gain", "capital_loss", "hours_per_week")
new_df_long <- new_df %>%
  select(all_of(numeric_cols)) %>%
  gather(key = "Variable", value = "Value")
ggplot(new_df_long, aes(x = Variable, y = Value)) +
  geom_boxplot(fill = "#3498db") +
  labs(title = "Boxplot of Numeric Columns (After Handling Outliers and Normalizing)", x = "Variable", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(nrow(new_df))

#
#-----------------------------------------------------
# log transformation (but didn't provided good result, so ignored this section)
numeric_cols <- sapply(new_df, is.numeric)
num_data <- new_df[, numeric_cols]
num_data_log_transformed <- log1p(num_data)
new_df[, numeric_cols] <- num_data_log_transformed
correlation_matrix_after_log <- cor(new_df[, numeric_cols])
print(correlation_matrix_after_log)

cor_matrix <- cor(new_df)
library(reshape2)
library(ggplot2)
melted_corr <- melt(cor_matrix)
cor_plot <- ggplot(data = melted_corr, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), vjust = 1) +  
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1),
        axis.text.y = element_text(size = 8)) +
  labs(x = "Variable 1", y = "Variable 2")
print(cor_plot)

#-----------------------------------------------------------
#
#
# correlation heatmap
n_cols <- sapply(new_df, is.numeric)
num_dt <- new_df[, n_cols]
c_matrix <- cor(num_dt)
library(reshape2)
library(ggplot2)
melt_corr <- melt(c_matrix)
c_plot <- ggplot(data = melt_corr, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), vjust = 1) +  
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1),
        axis.text.y = element_text(size = 8)) +
  labs(x = "Features", y = "Features")
print(c_plot)
#
#
# Visualizations
# Pie chart for income distribution
windows(width = 30, height = 30)
income_distribution <- table(cleaned_df$income)
pie(
  income_distribution,
 # labels = names(income_distribution),
  labels = NA,
  main = "Income Distribution",
  col = c("green", "yellow"),
  cex.main = 1.5  
)
label_positions <- cumsum(income_distribution) - income_distribution / 2
text(
  x = label_positions,
  y = rep(0, length(label_positions)),  
  labels = names(income_distribution),
  col = "white",  
  cex = 1.2,  
  pos = 3, 
  offset = 0.5  
)
#
#
# scatter plot for capital_gain and capital_loss
ggplot(cleaned_df, aes(x = capital_gain, y = capital_loss, color = factor(income))) +
  geom_point() +
  labs(title = "Scatter Plot of Capital Gain vs Capital Loss", x = "Capital Gain", y = "Capital Loss") +
  scale_color_manual(values = c("#3498db", "#e74c3c")) +
  theme_minimal()
# boxplot for capital_gain and capital_loss
ggplot(cleaned_df, aes(x = factor(income), y = capital_gain, fill = factor(income))) +
  geom_boxplot() +
  labs(title = "Box Plot of Capital Gain Loss by Income", x = "Capital Gain", y = "Capital Loss") +
  scale_fill_manual(values = c("#3498db", "#e74c3c")) +
  theme_minimal()
#
#
# Bar Plot
library(ggplot2)
barplot_plot <- ggplot(cleaned_df, aes(x = factor(income), fill = factor(income))) +
  geom_bar() +
  labs(title = "Bar Plot of Income Distribution", x = "Income", y = "Count") +
  scale_fill_manual(values = c("#3498db", "#e74c3c")) +
  theme_minimal()
print(barplot_plot)
#
#
# Modeling
# splitting of dataset
set.seed(22242821)
library(caret)
splitIndex <- createDataPartition(new_df$income, p = 0.7, list = FALSE)
train <- new_df[splitIndex, ]
test <- new_df[-splitIndex, ]
print(nrow(train))
print(nrow(test))

#
#
# random forest model
train$income <- as.factor(train$income)
rf_model <- randomForest(income ~ ., data = train, ntree = 100)
predictions_rf <- predict(rf_model, newdata = test)
conf_matrix_rf <- table(predictions_rf, test$income)
accuracy_rf <- sum(diag(conf_matrix_rf)) / sum(conf_matrix_rf)
print(paste("Accuracy for Random Forest:", round(accuracy_rf * 100, 2), "%"))
#
#
# xbboost model
set.seed(22242821)
x_train <- new_df[splitIndex, ]
x_test <- new_df[-splitIndex, ]
x_model <- xgboost(data = as.matrix(x_train[, -ncol(x_train)]),
                   label = x_train$income,nrounds = 100, objective = "binary:logistic",eval_metric = "logloss")
x_pred <- predict(x_model, as.matrix(x_test[, -ncol(x_test)]))
x_pred_class <- ifelse(x_pred > 0.5, 1, 0)
x_conf_matrix <- table(Actual = x_test$income, Predicted = x_pred_class)
x_accuracy <- sum(diag(x_conf_matrix)) / sum(x_conf_matrix)
print(paste("Accuracy for XGBoost:", round(x_accuracy * 100, 2), "%"))
#
#
# Evaluation of Models
# evaluation metrics for Random Forest
precision_rf <- conf_matrix_rf[2, 2] / sum(conf_matrix_rf[, 2])
recall_rf <- conf_matrix_rf[2, 2] / sum(conf_matrix_rf[2, ])
f1_score_rf <- 2 * (precision_rf * recall_rf) / (precision_rf + recall_rf)
print(paste("Precision for Random Forest:", round(precision_rf, 2)))
print(paste("Recall for Random Forest:", round(recall_rf, 2)))
print(paste("F1 Score for Random Forest:", round(f1_score_rf, 2)))
#
#
# evaluation metrics for XGBoost
precision_xgboost <- x_conf_matrix[2, 2] / sum(x_conf_matrix[, 2])
recall_xgboost <- x_conf_matrix[2, 2] / sum(x_conf_matrix[2, ])
f1_score_xgboost <- 2 * (precision_xgboost * recall_xgboost) / (precision_xgboost + recall_xgboost)
print(paste("Precision for XGBoost:", round(precision_xgboost, 2)))
print(paste("Recall for XGBoost:", round(recall_xgboost, 2)))
print(paste("F1 Score for XGBoost:", round(f1_score_xgboost, 2)))

