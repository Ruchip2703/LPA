#Imputing missing values in independent dataset (83 samples)
#Install and load the necessary packages
#install.packages(reshape2)
#install.packages(ggplot2)
#install.packages(dplyr)
#install.packages(readxl)
#install.packages(tidyLPA)
#install.packages(openxlsx)
#install.packages(readxl)
library(reshape2)
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyLPA)
library(openxlsx) 
library(readxl)

#LOAD DATA WITH MISSING VALUES
independent_data <- read_excel("C:/Users/Hp/Desktop/RUCHI/LPA/Random samples results/validation data.xlsx")
#Rename Columns
colnames(independent_data)[colnames(independent_data) == "variable to replace"] <- "what to replace with"

# Remove column "Class"
df1 <- results[, -which(names(results) == "Class")]
#Rename Columns
colnames(df1)[colnames(df1) == "variable to replace"] <- "what to replace with"

#merge each sample with missing value from independent dataset
#Find the row index where "EN" column has value "x"
#"EN" is the ID for each sample
#Repeat this step for each sample
row_index <- which(independent_data$EN == x)

# Check if a row with "EN" value "x" exists
if (length(row_index) > 0) {
  # Merge the row with df1
  dfx <- rbind(df1, independent_data[row_index, ])
} else {
  print("No row with EN value x found.")
}

set.seed(123)
dfx_imput <- dfx %>%
  subset(select = c("EN", "var1", "var2",...,"varN")) %>%
  single_imputation()


#Manually enter each imputed value for each sample in an excel file with all the independent samples
#Read excel file with imputed values
ext <- read_excel("path to excel file")
# Create a new data frame with the variables used to predict class
imp_ext <- select(ext, "var1", "var2",...,"varN")


set.seed(123)
# Make predictions on the external validation
P.ex <- predict(model, imp_ext, type = "prob")
predictions_ex <- ifelse(P.ex[,1] > 0.5, 1, 2)
predictions_ex <- factor(predictions_ex, levels = c("1", "2"))


# Combine predictions 
results.ex <- data.frame()
results.ex <- imp_ext %>%
  bind_cols(predictions_ex, ext$EN)
colnames(results.ex)[colnames(results.ex) == "variable to replace"] <- "what to replace with"
print(results.ex)

#Save prediction results in an excel file
write.xlsx(results.ex, file = "path where to save", rowNames = FALSE)

#Extract Class of each sample and merge it with the independent data in an excel file
predicted_data <- read_excel("path to saved file")
#rename columns
colnames(predicted_data)[colnames(predicted_data) == "variable to replace"] <- "what to replace with"

#Check if the predicted classes for samples show similar results as samples classified by LPA

#TESTS FOR CONTINUOUS VARIABLES
#Define the variables to exclude
exclude_vars <- c()

# List to store test results
Mann_Whitney_test_results_pred <- list()

# Loop through each numeric variable in the dataframe
for (col in names(predicted_data)) {
  # Skip non-numeric columns, the column containing class assignments, and the excluded variables
  if (!is.numeric(predicted_data[[col]]) || col == "Class" || col %in% exclude_vars) {
    next
  }
  
  # Perform Mann-Whitney test
  test_result <- wilcox.test(predicted_data[[col]] ~ predicted_data$Class, exact = FALSE)
  
  # Store test result in the list with the column name as the key
  Mann_Whitney_test_results_pred[[col]] <- test_result
}

# Create an empty dataframe to store results
MW_result_df_pred <- data.frame(Variable = character(), P_Value = numeric(), stringsAsFactors = FALSE)

# Loop through each test result
for (col in names(Mann_Whitney_test_results_pred)) {
  # Extract the p-value from the test result
  p_value <- Mann_Whitney_test_results_pred[[col]]$p.value
  
  # Add variable name and p-value to the result dataframe
  MW_result_df_pred <- rbind(MW_result_df_pred, data.frame(Variable = col, P_Value = p_value))
}


# Iterate over each element in the "P_Value" column and convert to numeric
MW_result_df_pred$P_Value <- sapply(MW_result_df_pred$P_Value, function(x) as.numeric(format(as.numeric(x), scientific = FALSE)))

# Print the result dataframe
print(MW_result_df_pred)

#save the dataframe as excel and read it
write.xlsx(MW_result_df_pred, file = "path where to save", rowNames = FALSE)
Mann_Whitney <- read_excel("path to saved file")



#TESTS FOR CATEGORICAL  VARIABLES
##chi square for categorical variables
# Select the columns you want to perform the chi-squared test on
selected_columns <- c("var1", "var2",...,"varN")

# Create a list to store the p-values for each column
p_values <- numeric(length(selected_columns))

# Perform the chi-squared test for each selected column 
for (i in seq_along(selected_columns)) {
  contingency_table <- table(predicted_data$Class, predicted_data[[selected_columns[i]]])
  chi_squared_test <- chisq.test(contingency_table, correct = FALSE)  
  p_values[i] <- chi_squared_test$p.value
}
# Create a dataframe to store the results
chi_square_results_pred <- data.frame(variable = selected_columns, p_value = p_values)

# Print the dataframe with the results
print(chi_square_results_pred)

#save the dataframe as excel and read it
write.xlsx(chi_square_results_pred, file = "path where to save", rowNames = FALSE)
chi_square_results_pred <- read_excel("path to saved file")


#CALCULATING MEANS
means_pred <- predicted_data %>%
  group_by(Class) %>%
  summarise_all(list(mean = ~mean(., na.rm = TRUE), sd = ~round(sd(., na.rm = TRUE), 3)))

# Display the means
print(means_pred)

#save the dataframe as excel 
write.xlsx(means_pred, file = "path where to save", rowNames = FALSE)
#format excel to contain columns - variables, class1 mean, class2 mean, sd1, sd2, p value
#Extract p value for each variable from it respective statistical test dataframe
#read the excel file
means_pred <- read_excel("path to saved file")


#VISUALIZATIONS
#CONDITIONS PREVALENCE

#create an excel for prevalence of each condition
#read excel file
prevalence_pred <- read_excel("path to saved file")

data_long <- melt(prevalence_pred, id.vars = "Variable")

# Define custom colors for each variable
custom_colors <- c("condition1" = "lightblue",
                   "condition2" = "coral",
                   "condition3" = "lightgreen",
                   "condition4" = "violet")

# Plot with ggplot2
ggplot(data_long, aes(x = variable, y = value, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", linewidth = 0.2) +  # Add outline color
  geom_text(aes(label = sprintf("%.2f%%", value), group = Variable),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3) +
  labs(title = "xyz",
       x = "Classes", y = "Prevalence (%)") +
  scale_fill_manual(values = custom_colors) +  # Use custom colors
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2))+
  ylim(0, 100)  # Set y-axis limits to show values up to 100


