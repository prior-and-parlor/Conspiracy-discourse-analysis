---
  title: "SLRM with Transformation and Robust Modelling"
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Install and load Libraries

```{r, warning=FALSE,message=FALSE}
# List of libraries to install
libraries_to_install <- c("car", "tidyverse", "MASS", "lmtest", "lme4", "glmmTMB", "sjPlot", "itsadug", "DHARMa","Matrix","sandwich","knitr","report","DT")

# Check if each library is installed, and install if not
for (library_name in libraries_to_install) {
  if (!requireNamespace(library_name, quietly = TRUE)) {
    install.packages(library_name, dependencies = TRUE)
  }
}

# Load the libraries
library(car)
library(tidyverse)
library(MASS)
library(lmtest)
library(lme4)
library(glmmTMB)
library(sjPlot)
library(itsadug)
library(DHARMa)
library(Matrix)
library(sandwich)
library(knitr)
library(report)
library(DT)

```

# Loading the data

```{r}
# Read the CSV file
RegressionData <- read.csv("RegressionDataR.csv")

# Checking the  variable names and head
DT::datatable(head(RegressionData), options = list(pageLength = 5), caption = "First Few Rows of the Data")
knitr::kable(names(RegressionData), caption = "Variable Names")
```

# Creating a list of all the emotions of interest and the data frame to store the results

```{r}
# List of emotions
emotions <- c("relative_anger", "relative_anticipation", "relative_disgust",
              "relative_fear", "relative_joy", "relative_sadness",
              "relative_surprise", "relative_trust", "relative_negative",
              "relative_positive")

# Create an empty data frame to store results
results_table <- data.frame(Model = character(), 
                            Shapiro_Wilk_p_value = numeric(), 
                            Breusch_Pagan_p_value = numeric(), 
                            stringsAsFactors = FALSE)

# List to store models
model_list <- list()
```

# Subsetting the data with specific topic and fitting SLRM

```{r}
# Loop over each emotion to fit models
for (emotion in emotions) {
  # Subset the data for the current emotion
  emotion_data <- RegressionData %>% filter(Topic == "CovidMasks")#Change to the Specific Topic as needed
  
  
  # Fit a simple linear regression model
  model <- lm(paste(emotion, "~ month"), data = emotion_data)
  
  # Store the model in the list
  model_list[[emotion]] <- model
}

```

# Checking for assumption violations

## **Normality and** Homoscedasticity **Checks**

```{r}
# Loop for normality and homoscedasticity checks
for (emotion in emotions) {
  # Extract the model from the list
  model <- model_list[[emotion]]
  
  # Checking for normality of the residuals
  # Residuals
  residuals <- residuals(model)
  
  # Normality of Residuals Test (Shapiro-Wilk)
  shapiro_test_result <- shapiro.test(residuals)
  shapiro_p_value <- shapiro_test_result$p.value
  
  # Homoscedasticity Test (Breusch-Pagan Test)
  bptest_result <- bptest(model, studentize = FALSE)
  bptest_p_value <- bptest_result$p.value
  
  # Append results to the table
  results_table <- rbind(results_table, 
                         data.frame(Model = emotion,
                                    Shapiro_Wilk_p_value = shapiro_p_value,
                                    Breusch_Pagan_p_value = bptest_p_value))
}


# Display the entire table using datatable
DT::datatable(results_table, caption = "Results Table")
```


## **Linearity Check**

```{r}
# Linearity Check
for (emotion in emotions) {
  # Extract the model from the list
  model <- model_list[[emotion]]
  
  # Linearity Check (Scatterplot of Residuals vs Fitted)
  ggplot(data = data.frame(Fitted = fitted(model), Residuals = residuals(model)),
         aes(x = Fitted, y = Residuals)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    ggtitle(paste("Residuals vs Fitted Plot for", emotion))
  
  # Print the  plots
  print(last_plot())
}

```

# Models that violated the assumption of SLRM

```{r}
# Filter models with p-values below 0.05 for either normality or homoscedasticity
violated_models <- results_table %>%
  filter(Shapiro_Wilk_p_value < 0.05 | Breusch_Pagan_p_value < 0.05)

# Print the violated models
if (nrow(violated_models) > 0) {
  cat("Models Violating Assumptions for SLRM")
  print(violated_models)
} else {
  cat("No models violating assumptions found.\n")
}


# Print the Models Violating Assumptions for SLRM using DT::datatable
if (nrow(violated_models) > 0) {
  cat("Models Violating Assumptions for SLRM")
  DT::datatable(violated_models, options = list(pageLength = 5), caption = "Models Violating Assumptions for SLRM")
} else {
  cat("No models violating assumptions found.\n")
}
```

# Transformation Techniques

This section automatically check The models that violated the assumptions and try to rectify them using the following transformation techniques :
  
  -   Log transform response

-   Square root transform response

-   Box-Cox transform response

-   Log transform predictor

This section automatically capture the model that violated the assumption, transform them using the 4 transformation methods and replace the original model with the best transformed model out of the four before generating the outputs.

```{r}
# Creating an empty data frame to store transformed results
transformed_results_table <- data.frame(Model = character(), 
                                        Shapiro_Wilk_p_value = numeric(), 
                                        Breusch_Pagan_p_value = numeric(), 
                                        stringsAsFactors = FALSE)

# Loop over all the 10 emotions
for (emotion in emotions) {
  # Checking for normality of the residuals
  # Residuals
  residuals <- residuals(model_list[[emotion]])
  
  #  Normality of Residuals Test (Shapiro-Wilk)
  shapiro_test_result <- shapiro.test(residuals)
  shapiro_p_value <- shapiro_test_result$p.value
  
  #  Homoscedasticity Test (Breusch-Pagan Test)
  bptest_result <- bptest(model_list[[emotion]], studentize = FALSE)
  bptest_p_value <- bptest_result$p.value
  
  # Checking if either p-value is less than 0.05
  if (shapiro_p_value < 0.05 | bptest_p_value < 0.05) {
    # here if Violation is  detected, apply transformations and fit new models
    
    # Log transform response
    log_model <- lm(log(get(emotion)) ~ month, data = emotion_data)
    
    # Square root transform response
    sqrt_model <- lm(sqrt(get(emotion)) ~ month, data = emotion_data)
    
    # Box-Cox transform response
    lambda <- boxcox(lm(get(emotion) ~ month, data = emotion_data))$x[which.max(boxcox(lm(get(emotion) ~ month, data = emotion_data))$y)]
    transformed_response <- (emotion_data[[emotion]]^lambda - 1) / lambda
    box_cox_model <- lm(transformed_response ~ month, data = emotion_data)
    
    # Log transform predictor
    log_predictor_model <- lm(get(emotion) ~ log(month), data = emotion_data)
    
    # Save the transformed models with different names to avoid inconviniences with the original models
    assign(paste("log_", emotion, "_model", sep = ""), log_model)
    assign(paste("sqrt_", emotion, "_model", sep = ""), sqrt_model)
    assign(paste("box_cox_", emotion, "_model", sep = ""), box_cox_model)
    assign(paste("log_predictor_", emotion, "_model", sep = ""), log_predictor_model)
    
    # Re-run normality and homoscedasticity tests for transformed models
    # Log transform
    log_residuals <- residuals(log_model)
    log_shapiro_p_value <- shapiro.test(log_residuals)$p.value
    log_bptest_p_value <- bptest(log_model, studentize = FALSE)$p.value
    
    # Square root transform
    sqrt_residuals <- residuals(sqrt_model)
    sqrt_shapiro_p_value <- shapiro.test(sqrt_residuals)$p.value
    sqrt_bptest_p_value <- bptest(sqrt_model, studentize = FALSE)$p.value
    
    # Box-Cox transform
    box_cox_residuals <- residuals(box_cox_model)
    box_cox_shapiro_p_value <- shapiro.test(box_cox_residuals)$p.value
    box_cox_bptest_p_value <- bptest(box_cox_model, studentize = FALSE)$p.value
    
    # Log transform predictor
    log_predictor_residuals <- residuals(log_predictor_model)
    log_predictor_shapiro_p_value <- shapiro.test(log_predictor_residuals)$p.value
    log_predictor_bptest_p_value <- bptest(log_predictor_model, studentize = FALSE)$p.value
    
    # Append the results to the transformed table
    transformed_results_table <- rbind(transformed_results_table, 
                                       data.frame(Model = paste("log_", emotion, "_model", sep = ""),
                                                  Shapiro_Wilk_p_value = log_shapiro_p_value,
                                                  Breusch_Pagan_p_value = log_bptest_p_value),
                                       data.frame(Model = paste("sqrt_", emotion, "_model", sep = ""),
                                                  Shapiro_Wilk_p_value = sqrt_shapiro_p_value,
                                                  Breusch_Pagan_p_value = sqrt_bptest_p_value),
                                       data.frame(Model = paste("box_cox_", emotion, "_model", sep = ""),
                                                  Shapiro_Wilk_p_value = box_cox_shapiro_p_value,
                                                  Breusch_Pagan_p_value = box_cox_bptest_p_value),
                                       data.frame(Model = paste("log_predictor_", emotion, "_model", sep = ""),
                                                  Shapiro_Wilk_p_value = log_predictor_shapiro_p_value,
                                                  Breusch_Pagan_p_value = log_predictor_bptest_p_value))
  }
}

# Print the transformed models' results table
if (nrow(transformed_results_table) > 0) {
  print("Transformed Models Results Table:")
  print(transformed_results_table)
} else {
  print("No transformed models found.")
}




# Print the transformed models' results table using DT::datatable
if (nrow(transformed_results_table) > 0) {
  cat("Transformed Models Results Table:")
  DT::datatable(transformed_results_table, options = list(pageLength = 5), caption = "Transformed Models Results Table")
} else {
  print("No transformed models found.")
}

```

When the violation occurs and no transformation method rectifies this, the code will proceed to fit a robust model using MASS package; the **`MASS::rlm`** function in R is used to fit a linear regression model using M-estimation with Huber's proposal 2, this type of model is considered robust to outliers and a more robust alternative to standard OLS. It is less sensitive to the influence of individual data points (that deviate from the overall pattern of the data which may lead to non-normality or heteroscedasticity). In the event that no model violated any of the assumptions, the code will skip all the other stages and will proceed to automatically generate outputs.

```{r,results='asis'}
# List to store final models
final_model_list <- list()
report_outputs <- list()

# Loop over the 10 emotions
for (emotion in emotions) {
  # Extract the original model
  original_model <- model_list[[emotion]]
  
  # Check if the model violated assumptions
  if (any(violated_models$Model == emotion)) {
    # Check if the transformed model exists
    transformed_model <- transformed_results_table[transformed_results_table$Model == emotion, ]
    
    # Check if the p-value from the transformed model is > 0.05
    if (any(transformed_model$Shapiro_Wilk_p_value > 0.05 | transformed_model$Breusch_Pagan_p_value > 0.05)) {
      # Replace with the transformed model having the highest p-value
      max_p_value_model <- transformed_model[which.max(transformed_model$Shapiro_Wilk_p_value), ]
      final_model_list[[emotion]] <- get(max_p_value_model$Model)
    } else {
      # Fit a robust model in place of SLRM if the tranformed model did not meet the conditions above
      robust_model <- MASS::rlm(formula = formula(original_model), data = model.frame(original_model))
      final_model_list[[emotion]] <- robust_model
    }
  } else {
    # Keep the original model if it didn't violate assumptions
final_model_list[[emotion]] <- original_model
}

# Store the report output
report_outputs[[emotion]] <- report(final_model_list[[emotion]], output = "html")
}

# Print the stored outputs 
for (emotion in emotions) {
  
  cat("Model Summary for", emotion, ":\n")
  cat(report_outputs[[emotion]])
  cat("\n\n")
}
```

# **Creating Table of Results  Each Model**

```{r}
# Access individual models by emotion name
anger_model <- final_model_list[[1]]
anticipation_model <- final_model_list[[2]]
disgust_model <- final_model_list[[3]]
fear_model <- final_model_list[[4]]
joy_model <- final_model_list[[5]]
sadness_model <- final_model_list[[6]]
surprise_model <- final_model_list[[7]]
trust_model <- final_model_list[[8]]
negative_model <- final_model_list[[9]]
positive_model <- final_model_list[[10]]


sjPlot::tab_model(anger_model)

sjPlot::tab_model(anticipation_model)

sjPlot::tab_model(disgust_model)

sjPlot::tab_model(fear_model)

sjPlot::tab_model(joy_model)

sjPlot::tab_model(sadness_model)

sjPlot::tab_model(surprise_model)

sjPlot::tab_model(trust_model)

sjPlot::tab_model(negative_model)

sjPlot::tab_model(positive_model)



```
