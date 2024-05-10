#get raw data
library(data.table)
library(ggplot2)
library(dplyr)
library(rpart)
library(rpart.plot)
library(randomForest)
alldata <- fread("ac.csv", na.strings = c("N/A", "-"))
subdata <- alldata[grep("Australia",alldata$Sold_in),]

# subset with data
data_with_values <- subdata[!is.na(subdata$AnnualOutputEER) & !is.na(subdata$AnnualOutputCOP),]

# subset without data
data_without_values <- subdata[is.na(subdata$AnnualOutputEER) | is.na(subdata$AnnualOutputCOP),]

missing_values_table <- table(is.na(subdata$`Rated AEER`), useNA = "ifany")

# Calculate the proportion of non-missing values in each column
non_missing_percentage <- 100 * colSums(!is.na(data_with_values)) / nrow(data_with_values)

data_delete_miss <- data_with_values[, .SD, .SDcols=which(non_missing_percentage > 70)]


columns_to_extract <- c("Registration Number","C-Dehumid_Rated", "C-Power_Inp_Rated", 
                        "C-Sens_Cool_Rated", "C-Total Cool Rated", "H-Power_Inp_Rated", "H-Total Heat Rated", 
                        "EERtestAvg", "COPtestAvg", "Rated cooling power input kW","Rated heating power input kW", "Pnoc","Pnoh",
                        "Rated AEER", "AnnualOutputEER", "AnnualOutputCOP")

selected_data <- data_delete_miss[, ..columns_to_extract]


selected_data <- unique(selected_data, by = "Registration Number")

selected_data[, `Registration Number` := NULL]


selected_data[] <- lapply(selected_data, function(x) {

  if (is.numeric(x)) {
    x[x == 0] <- NA
  }
  return(x)
})
#missing value
missing_percentage_select <- 100 * colSums(is.na(selected_data)) / nrow(selected_data)
print(missing_percentage_select)



missing_data <- data.frame(ColumnName = names(missing_percentage_select), MissingPercentage = missing_percentage_select)




ggplot(missing_data, aes(x = ColumnName, y = MissingPercentage, fill = MissingPercentage)) +
  geom_bar(stat = "identity") +  # 使用identity告诉ggplot这是预先计算好的统计数据
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # 旋转x轴标签以便阅读
  labs(x = "Column Name", y = "Percentage of Missing Values (%)", title = "Missing Data Percentage by Column") +
  scale_fill_gradient(low = "blue", high = "red")  # 使用渐变色填充



# outlier
selected_data <- selected_data %>%
  filter(!apply(., 1, function(x) any(x > 100)))




selected_data <- na.omit(selected_data)

library(caret)
set.seed(123)  #seed
# train_test_data
partition <- createDataPartition(selected_data$AnnualOutputEER, p = 0.8, list = FALSE)
train_data <- selected_data[partition, ]
test_data <- selected_data[-partition, ]


#stepwise

selected_data.full <- lm(AnnualOutputEER ~ . - AnnualOutputCOP , data = selected_data)
step_model_cool <- step(selected_data.full, direction = "both")
summary(step_model_cool)
selected_data.full2 <- lm(AnnualOutputCOP ~ . - AnnualOutputEER , data = selected_data)
step_model_heat <- step(selected_data.full2, direction = "both")

# Print the optimized heat model summary
summary(step_model_heat)


#AnnualOutputEER_regressiontree：
tree_model_cool <- rpart(AnnualOutputEER ~ . - AnnualOutputCOP, data = selected_data)

#Print and analyze regression tree model summary for AnnualOutputEER：

summary(tree_model_cool)
rpart.plot(tree_model_cool, type=5, extra=100)
summary(tree_model_heat)
rpart.plot(tree_model_heat, type=5, extra=100)


tree_model_heat <- rpart(AnnualOutputCOP ~ . - AnnualOutputEER, data = selected_data)

summary(tree_model_heat)
rpart.plot(tree_model_heat)


cool_summary <- summary(tree_model_cool)
print(cool_summary)


#for test_set
predictions_cool <- predict(tree_model_cool, selected_data)
SST_cool <- sum((selected_data$AnnualOutputEER - mean(selected_data$AnnualOutputEER))^2)
SSE_cool <- sum((selected_data$AnnualOutputEER - predictions_cool)^2)
Rsquared_cool <- 1 - SSE_cool / SST_cool
print(Rsquared_cool)


predictions_heat <- predict(tree_model_heat, selected_data)
SST_heat <- sum((selected_data$AnnualOutputCOP - mean(selected_data$AnnualOutputCOP))^2)
SSE_heat <- sum((selected_data$AnnualOutputCOP - predictions_heat)^2)
Rsquared_heat <- 1 - SSE_heat / SST_heat
print(Rsquared_heat)


#for test set
predictions <- predict(tree_model_cool, test_data)  # 根据你的模型类型更改模型名

# MSE
mse_EER <- mean((test_data$AnnualOutputEER - predictions)^2)  # 更改 AnnualOutputEER 为相应的响应变量名

# SST_SSE
SST_EER <- sum((test_data$AnnualOutputEER - mean(test_data$AnnualOutputEER))^2)
SSE_EER <- sum((test_data$AnnualOutputEER - predictions)^2)

# R-squared
rsquared_EER <- 1 - SSE_EER / SST_EER

# result
print(paste("MSE on test set EER:", mse_EER))
print(paste("R-squared on test set EER:", rsquared_EER))



predictions <- predict(tree_model_heat, test_data)  # 根据你的模型类型更改模型名


mse_COP <- mean((test_data$AnnualOutputCOP - predictions)^2)  # 更改 AnnualOutputEER 为相应的响应变量名


SST_COP <- sum((test_data$AnnualOutputCOP - mean(test_data$AnnualOutputCOP))^2)
SSE_COP <- sum((test_data$AnnualOutputCOP - predictions)^2)


rsquared_COP <- 1 - SSE_COP / SST_COP


print(paste("MSE on test set COP:", mse_COP))
print(paste("R-squared on test set COP:", rsquared_COP))

