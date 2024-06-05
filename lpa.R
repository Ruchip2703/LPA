# Install and load the necessary packages
#install.packages(dplyr)
#install.packages(readxl)
#install.packages(tidyLPA)
#install.packages(openxlsx)
#install.packages(tidyr)
#install.packages(corrplot)
#install.packages(ggcorrplot)
#install.packages(ggplot2)
#install.packages(reshape2)

library(dplyr)
library(readxl)
library(tidyLPA)
library(openxlsx) 
library(tidyr) 
library(corrplot)
library(ggcorrplot)
library(ggplot2)
library(reshape2)

#LOAD DATA WITH SAMPLES AND THEIR CLINICAL PARAMETERS
data <- read_excel("path to the file")
# Rename Columns
colnames(data)[colnames(data) == "variable to replace"] <- "what to replace with"

# Convert a specific column to numeric
column_to_convert <- 'column to convert into numeric'
data <- data %>%
  mutate(across(all_of(column_to_convert), as.numeric))


#Correlation
# Select the variables of interest
input_variables <- c("var1", "var2",...,"varN")

# Create a subset of the data with only selected variables
input_variables_df <- data[, input_variables, drop = FALSE]

# Create a correlation matrix
cor_matrix <- cor(input_variables_df, use = "complete.obs", method = "spearman")

#p-value of corrplot
pval.mtx <- cor_pmat(
  cor_matrix,
  vars = NULL,
  method = "spearman",
  conf.level = 0.95
)  

# Set wider margins for the plot
# Margins (bottom, left, top, right)
par(mar = c(0, 0, 0, 0))

# Print the correlation matrix 
corrplot(cor_matrix, method = "square", tl.col = "black", tl.cex = 0.9, 
         p.mat = pval.mtx, sig.level = 0.05, insig = "blank",
         addgrid = TRUE,
         main = "\n Correlation Matrix of Variables for LPA",
         mar = c(0, 0, 0, 0)) 

#OR

color_scheme <- c("red", "white", "blue")  # Blue, White, Red

# Print the correlation matrix
ggcorrplot(cor_matrix, col = color_scheme, p.mat = pval.mtx, method = "square")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12))

#OR

library(PerformanceAnalytics)
chart.Correlation(cor_matrix,
                  method="spearman",
                  histogram=TRUE,
                  pch=16)


#LPA
#subject non-collinear variables 
set.seed(123)
model_1<- data %>%
  subset(select = c("var1", "var2",...,"varN")) %>%
  single_imputation() %>%
  estimate_profiles(1:9, 
                    variances = c("equal"),
                    covariances = c("zero"))
model_1
plot(model_1)
get_data(model_1)

model_2 <- data %>%
  subset(select = c("var1", "var2",...,"varN")) %>%
  single_imputation() %>%
  estimate_profiles(1:9, 
                    variances = c("varying"),
                    covariances = c("zero"))
model_2 
get_data <- get_data(model_2)
plot_profiles(model_2[[3]])
plot(model_2)
get_estimates(model_2)
get_fit(model_2)

model_3 <- data %>%
  subset(select = c("var1", "var2",...,"varN")) %>%
  single_imputation() %>%
  estimate_profiles(1:9, 
                    variances = c("equal"),
                    covariances = c("equal"))
model_3
plot(model_3)
get_data(model_3)

model_6 <- data %>%
  subset(select = c("var1", "var2",...,"varN"))%>%
  single_imputation() %>%
  estimate_profiles(1:9, 
                    variances = c("varying"),
                    covariances = c("varying"))
model_6
plot(model2)
get_data(model_6)

#Create excel with all the models information
#Read excel with all the models information
#plot all models
all_models <- read_excel("path to excel file")
# Reshape the data to long format
data_longer <- pivot_longer(all_models, cols = -Classes, names_to = "Model", values_to = "BIC (lower is better)")

# Convert Variables to factor with custom order
data_longer$Classes <- factor(data_longer$Classes)

# Create line chart with custom ordering
ggplot(data_longer, aes(x = factor(Classes), y = `BIC (lower is better)`, color = Model, group = Model)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 15)) +
  theme(axis.text.x = element_text(hjust = 0.5, vjust = 1, size = 11),
        axis.text.y = element_text(hjust = 0.5, vjust = 0.5, size = 11),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "LPA Results",
       x = "Classes",
       y = "BIC (lower is better)",
       color = "Model")


# Assuming model_2 is the result of tidyLPA
#create dataframe of model_2
model_2_df <- data.frame()

# Print the dataframe
print(model_2_df)
write.xlsx(model_2_df, file = "path where to save the dataframe", rowNames = FALSE)

#Extract class assignment for the samples from the most suitable model
classes <- get_data[901:1200, ]

#Save as excel on your computer
write.xlsx(classes, file = "path where to save", rowNames = FALSE)

#merge excel files with sample details and class assignments
#read the excel file
results <- read_excel("path to your saved file")

#Shapiro-Wilk
# Extract numeric variables
numeric_vars <- names(results)[sapply(results, is.numeric)]
# Specify the variables to exclude
exclude_vars <- c("variables to exclude")

# Extract unique class labels from the "Class" column
classes <- unique(results$Class)

# Create an empty dataframe to store the results
shapiro_results <- data.frame(Variable = character(), Class = character(), W = numeric(), p_value = numeric(), stringsAsFactors = FALSE)

# Perform Shapiro-Wilk test for each numeric variable and each class
for (var in numeric_vars) {
  # Skip Shapiro-Wilk test for excluded variables
  if (var %in% exclude_vars) {
    cat("Variable", var, "excluded from Shapiro-Wilk test.\n\n")
    next  # Skip to the next iteration
  }
  
  cat("Shapiro-Wilk Test for Variable:", var, "\n")
  
  # Iterate over each class
  for (class in classes) {
    cat("Class:", class, "\n")
    
    # Extract data for the current class
    class_data <- results[results$Class == class,][[var]]
    
    # Check if the variable contains only numeric values and has sufficient variation
    if (all(sapply(class_data, is.numeric)) && length(unique(class_data)) > 1) {
      # Apply Shapiro-Wilk test
      shapiro_test <- shapiro.test(class_data)
      
      # Store test results in the dataframe
      shapiro_results <- rbind(shapiro_results, data.frame(Variable = var, Class = class, W = shapiro_test$statistic, p_value = shapiro_test$p.value))
    } else {
      cat("Variable does not meet criteria for Shapiro-Wilk test.\n")
    }
    
    cat("\n")
  }
}


# Print the dataframe with test results
print(shapiro_results)
#save the shapiro wilk results in an exel file
write.xlsx(shapiro_results, file = "path where to save", rowNames = FALSE)
#read the excel file
shapiro_results <- read_excel("path to saved file")


# Plot density curves for each class 
# Plot density plot
ggplot(results, aes(x = "var_to_plot", fill = factor(Class))) +
  geom_density(alpha = 0.5) +
  labs(title = "",
       x = "var_to_plot",
       y = "Density",
       fill = "Class") +
  theme_minimal()


#TESTS FOR CONTINUOUS VARIABLES

# Mann-Whitney Test for non-parametric variables
# Define the variables to exclude
exclude_vars <- c("var1", "var2",...,"varN")

# List to store test results
Mann_Whitney_test_results <- list()

# Loop through each numeric variable in the dataframe
for (col in names(results)) {
  # Skip non-numeric columns, the column containing class assignments, and the excluded variables
  if (!is.numeric(results[[col]]) || col == "Class" || col %in% exclude_vars) {
    next
  }
  
  # Perform Mann-Whitney test
  Mann_Whitney_test_result <- wilcox.test(results[[col]] ~ results$Class, exact = FALSE)
  
  # Store test result in the list
  Mann_Whitney_test_results[[col]] <- Mann_Whitney_test_result
}

# Create an empty dataframe to store results
MW_result_df <- data.frame(Variable = character(), P_Value = numeric(), stringsAsFactors = FALSE)

# Loop through each test result
for (col in names(Mann_Whitney_test_results)) {
  # Extract the p-value from the test result
  p_value <- Mann_Whitney_test_results[[col]]$p.value
  
  # Add variable name and p-value to the result dataframe
  MW_result_df <- rbind(MW_result_df, data.frame(Variable = col, P_Value = p_value))
}

# Iterate over each element in the "P_Value" column and convert to numeric
MW_result_df$P_Value <- sapply(MW_result_df$P_Value, function(x) as.numeric(format(as.numeric(x), scientific = FALSE)))

# Print the result dataframe
print(MW_result_df)

#save the dataframe as excel and read it
write.xlsx(MW_result_df, file = "path where to save", rowNames = FALSE)
Mann_Whitney <- read_excel("path to saved file")


#t-test for parametric variables
# Define the variables of interest
variables_of_interest <- c("var1", "var2",...,"varN")

# Create an empty dataframe to store results
t_test_results <- data.frame(Variable = character(), Class = character(), P_Value = numeric(), stringsAsFactors = FALSE)

# Perform class-wise t-tests for each variable
for (variable in variables_of_interest) {
  # Perform t-test for the current variable
  t_test_result <- t.test(results[[variable]] ~ results$Class)
  
  # Extract p-value
  p_value <- t_test_result$p.value
  
  # Store the results in the dataframe
  t_test_results <- rbind(t_test_results, data.frame(Variable = variable,  P_Value = p_value))
}

# Print the result dataframe
print(t_test_results)

#save the dataframe as excel and read it
write.xlsx(t_test_results, file = "path where to save", rowNames = FALSE)
t_test_results <- read_excel("path to saved file")

#################################################################################
#TESTS FOR CATEGORICAL  VARIABLES

##chi square for categorical variables
# Select the columns you want to perform the chi-squared test on
selected_columns <- c("var1", "var2",...,"varN")

# Create a list to store the p-values for each column
p_values <- numeric(length(selected_columns))

# Perform the chi-squared test for each selected column without Yates' correction
for (i in seq_along(selected_columns)) {
  contingency_table <- table(results$Class, results[[selected_columns[i]]])
  chi_squared_test <- chisq.test(contingency_table, correct = FALSE)  # Disable Yates' correction
  p_values[i] <- chi_squared_test$p.value
}
# Create a dataframe to store the results
chi_square_results <- data.frame(variable = selected_columns, p_value = p_values)

# Print the dataframe with the results
print(chi_square_results)
#save the dataframe as excel and read it
write.xlsx(chi_square_results, file = "path where to save", rowNames = FALSE)
chi_square_results <- read_excel("path to saved file")


#CALCULATING MEANS
means <- results %>%
  group_by(Class) %>%
  summarise_all(list(mean = ~mean(., na.rm = TRUE), sd = ~round(sd(., na.rm = TRUE), 3)))

# View the summary with mean and sd values by class
print(means)
#save the dataframe as excel and read it
write.xlsx(means, file = "path where to save", rowNames = FALSE)
#format excel to contain columns - variables, class1 mean, class2 mean, sd1, sd2, p value
#Extract p value for each variable from it respective statistical test dataframe
means <- read_excel("path to saved file")


#VISUALIZATIONS
#CONDITIONS PREVALENCE

#create an excel for prevalence of each condition
#read excel file
conditons_prevalence <- read_excel("prevalence excel file")


#CLASS-WISE BARPLOT FOR COMPLICATIONS
# Reshape the data for ggplot2
library(reshape2)
data_long <- melt(conditons_prevalence, id.vars = "Variables")

# Define custom colors for each variable
custom_colors <- c("condition1" = "lightblue",
                   "condition2" = "coral",
                   "condition3" = "lightgreen",
                   "condition4" = "violet")

# Plot with ggplot2
library(ggplot2)
ggplot(data_long, aes(x = variable, y = value, fill = Variables)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", linewidth = 0.2) +  # Add outline color
  geom_text(aes(label = sprintf("%.2f%%", value), group = Variables),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3) +
  labs(title = "\n Conditions Prevalence by Class",
       x = "Classes", y = "Prevalence (%)") +
  scale_fill_manual(values = custom_colors) +  # Use custom colors
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2))+
  ylim(0, 80)  # Set y-axis limits to show values up to 100


#COMPLICATIONS-WISE BARPLOT
# Select the variables to plot
variables_to_plot <- c("condition1", "condition2", 
                       "condition3", "condition4") 

# Filter the dataframe based on selected variables
data_selected <- means %>%
  filter(Variable %in% variables_to_plot)

data_selected <- data_selected %>%
  mutate(across(c(2, 3), ~ . * 100))

# Reshape the data to long format
data2_long <- data_selected %>%
  pivot_longer(cols = c(Class1, Class2), names_to = "Class", values_to = "Value")

# Reorder the levels of the 'Variables' factor based on their current order in the dataframe
data2_long$Variable <- factor(data2_long$Variable, levels = unique(data2_long$Variable))

# Get unique variables and corresponding p-values
unique_variables <- unique(data2_long$Variable)
unique_p_values <- numeric(length(unique_variables))

# Loop through each unique variable
for (i in seq_along(unique_variables)) {
  # Extract the p-values corresponding to the current variable
  p_values_for_variable <- data2_long$p_value[data2_long$Variable == unique_variables[i]]
  # Take the unique p-value (assuming both classes have the same p-value)
  unique_p_values[i] <- unique(p_values_for_variable)
}

# Print the unique p-values
unique_p_values

# Extract unique variables and p-values
unique_variables <- unique(data2_long$Variable)
unique_p_values <- tapply(data2_long$p_value, data2_long$Variable, min)

# Plot the bar plot with error bars and p-value annotations
ggplot(data2_long, aes(x = Variable, y = Value, fill = Class)) +
  geom_bar(stat = "identity", color = "black", position = "dodge", width = 0.7) +
  geom_text(aes(label = sprintf("%.2f%%", Value), group = Class),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 3) +
  labs(title = "\n Conditions Prevalence by Class",
       x = "Variables", y = "Values",
       fill = "Class") +
  scale_fill_manual(values = c("Class1" = "lightblue", "Class2" = "lightgreen")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.3, size = 10.5),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(margin = margin(b = 10)),
        axis.title.x = element_text(margin = margin(t = 10))) +
  ylim(0, 100) +  # Remove y-axis limit
  annotate("text", x = unique_variables, y = 80, 
           label = ifelse(unique_p_values < 0.05, 
                          ifelse(unique_p_values < 0.01, 
                                 ifelse(unique_p_values < 0.001, "***", "**"), "*"), ""),
           size = 5, color = "black", vjust = 0)  # Add asterisks for significant p-values



#PLOT WITH ERROR BARS
# Select the variables to plot
variables_to_plot <- c("var1", "var2", ... , "varN") 

# Filter the dataframe based on selected variables
data2_selected <- means %>%
  filter(Variable %in% variables_to_plot)

# Reshape the data to long format
data2_long <- data2_selected %>%
  pivot_longer(cols = c(Class1, Class2), names_to = "Class", values_to = "Value") %>%
  mutate(SD = case_when(
    Class == "Class1" ~ SD1,
    Class == "Class2" ~ SD2,
    TRUE ~ NA_real_  # Handle other cases (if any)
  ))
# Reorder the levels of the 'Variables' factor based on their current order in the dataframe
data2_long$Variable <- factor(data2_long$Variable, levels = unique(data2_long$Variable))

# Get unique variables and corresponding p-values
unique_variables <- unique(data2_long$Variable)
unique_p_values <- numeric(length(unique_variables))

# Loop through each unique variable
for (i in seq_along(unique_variables)) {
  # Extract the p-values corresponding to the current variable
  p_values_for_variable <- data2_long$p_value[data2_long$Variable == unique_variables[i]]
  # Take the unique p-value (assuming both classes have the same p-value)
  unique_p_values[i] <- unique(p_values_for_variable)
}

# Print the unique p-values
unique_p_values

# Extract unique variables and p-values
unique_variables <- unique(data2_long$Variable)
unique_p_values <- tapply(data2_long$p_value, data2_long$Variable, min)


data2_long$Variable <- as.factor(data2_long$Variable)
# Custom labels
custom_labels <- c("var1", "var2", ... , "varN")
#PLOT
ggplot(data2_long, aes(x = Variable, y = Value, fill = Class)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = pmax(0, Value - SD), ymax = Value + SD),
                position = position_dodge(width = 0.7), width = 0.2) +
  labs(title = "Comparing the Two Classes",
       x = "Variables", y = "Values",
       fill = "Class") +
  scale_fill_manual(values = c("Class1" = "grey70", "Class2" = "#78829E")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3, size = 10.5),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(margin = margin(b = 10)),
        axis.title.x = element_text(margin = margin(t = 10))) +
  scale_x_discrete(labels = custom_labels) +  # Apply custom labels
  ylim(0, 40) +  # Remove y-axis limit
  annotate("text", x = unique_variables, y = 25, 
           label = ifelse(unique_p_values < 0.05, 
                          ifelse(unique_p_values < 0.01, 
                                 ifelse(unique_p_values < 0.001, "***", "**"), "*"), ""),
           size = 5, color = "black", vjust = 0)  # Add asterisks for significant p-values
