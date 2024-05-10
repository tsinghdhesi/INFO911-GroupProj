#get raw data
library(data.table)
library(ggplot2)
alldata <- fread("ac.csv", na.strings = c("N/A", "-"))
subdata <- alldata[grep("Australia",alldata$Sold_in),]
# Define a function to replace "N/A" only for non-date columns

# subset with data
data_with_values <- subdata[!is.na(subdata$sri2010_cool) & !is.na(subdata$sri2010_heat),]

# No subset of data
data_without_values <- subdata[is.na(subdata$sri2010_cool) | is.na(subdata$sri2010_heat),]

missing_values_table <- table(is.na(subdata$`Rated AEER`), useNA = "ifany")



# Calculate the proportion of non-missing values in each column
non_missing_percentage <- 100 * colSums(!is.na(data_with_values)) / nrow(data_with_values)
#More than 70% is considered valid data
data_delete_miss <- data_with_values[, .SD, .SDcols=which(non_missing_percentage > 70)]
#Extract potentially relevant variables from columns with valid values exceeding 70%

columns_to_extract <- c("Registration Number", "C-Dehumid_Rated", "Configuration2", 
                        "C-Sens_Cool_Rated", "C-Total Cool Rated", "H-Power_Inp_Rated", "H-Total Heat Rated", 
                        "EERtestAvg", "COPtestAvg", "AnnualOutputEER", "AnnualOutputCOP", 
                        "Submit_ID", "EER", "Rated cooling power input kW", "Pnoc","Pnoh",
                        "Rated heating power input kW", "Rated AEER", "Rated ACOP", "sri2010_cool", "sri2010_heat")

#Extract these columns
selected_data <- data_delete_miss[, ..columns_to_extract]



#Change all 0s in select_data to NA
# Assume select_data is the loaded data.table object
# Replace all 0's with NA
selected_data[] <- lapply(selected_data, function(x) {

  if (is.numeric(x)) {
    x[x == 0] <- NA
  }
  return(x)
})
selected_data <- selected_data[!(sri2010_cool == 0 & sri2010_heat == 0), ]
#View missing values
missing_percentage_select <- 100 * colSums(is.na(selected_data)) / nrow(selected_data)
print(missing_percentage_select)




#Draw a graph of missing values
# Convert to data frame for easy use of ggplot
missing_data <- data.frame(ColumnName = names(missing_percentage_select), MissingPercentage = missing_percentage_select)




# Draw a histogram
ggplot(missing_data, aes(x = ColumnName, y = MissingPercentage, fill = MissingPercentage)) +
  geom_bar(stat = "identity") +  
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  labs(x = "Column Name", y = "Percentage of Missing Values (%)", title = "Missing Data Percentage by Column") +
  scale_fill_gradient(low = "blue", high = "red")  # 

# Remove duplicate rows
selected_data <- unique(selected_data, by = "Registration Number")
#Delete submit_id
selected_data[, Submit_ID := NULL]
#View missing values
missing_percentage_select <- 100 * colSums(is.na(selected_data)) / nrow(selected_data)
print(missing_percentage_select)

#Complete missing values
# Calculate the median of C-Dehumid_Rated, ignoring NA
median_c_dehumid <- median(selected_data$'C-Dehumid_Rated', na.rm = TRUE)

# Use median to fill missing values
selected_data$'C-Dehumid_Rated'[is.na(selected_data$'C-Dehumid_Rated')] <- median_c_dehumid

# Check the filled results
print(table(is.na(selected_data$'C-Dehumid_Rated')))






selected_data$Configuration2 <- factor(selected_data$Configuration2,
                                       levels = c("Window Wall", "Single Split System"))





#Delete the first column

selected_data <- selected_data[ , -1, with = FALSE]








# Convert 'Configuration2' to a factor with specified levels
selected_data$Configuration2 <- factor(selected_data$Configuration2,
                                       levels = c("Window Wall", "Single Split System"))

# Create dummy variables for 'Configuration2'
configuration_dummies <- model.matrix(~ Configuration2 - 1, data = selected_data)

# Convert to a data frame and ensure column names are appropriate
configuration_dummies <- as.data.table(configuration_dummies)

# Merge dummy variables back into the main data frame
selected_data <- cbind(selected_data, configuration_dummies)




# Delete all rows with missing values
selected_data <- na.omit(selected_data)

# Build a model for sri2010_cool and remove unnecessary variables # Use stepwise regression to optimize the cool model
selected_data.full <- lm(sri2010_cool ~ . - sri2010_heat - Configuration2, data = selected_data)

# Use stepwise regression to optimize the cool model
step_model_cool <- step(selected_data.full, direction = "both")

#Print optimized cool model summary
summary(step_model_cool)

# Build a model for sri2010_heat and remove unnecessary variables
selected_data.full2 <- lm(sri2010_heat ~ . - sri2010_cool - Configuration2, data = selected_data)

# Use stepwise regression to optimize the heat model
step_model_heat <- step(selected_data.full2, direction = "both")

#Print the optimized heat model summary
summary(step_model_heat)
