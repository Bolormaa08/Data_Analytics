#https://catalog.data.gov/dataset/heart-disease-mortality-data-among-us-adults-35-by-state-territory-and-county?fbclid=IwAR1GMrbyJaxzeq8nIFd5m-nA3cuhdnF0mFhQu0sM-rzLbHBDjUHH0PZ-5W4
install.packages("rpart.plot")
install.packages("pROC")
install.packages("rms")
install.packages("olsrr")
install.packages("effects")
detach("package:car", unload = TRUE)
install.packages("car")
install.packages("ggeffects")  # Install the package if you haven't already
install.packages("rms")



library(stats)
library(rpart)
library(MLmetrics)
library(rpart.plot)
library(pROC)
library(caret)
library(rms)
library(pROC)  # For ROC curve
library(rms)   # For calibration curve
library(olsrr) # For diagnostic plots
library(car)   # For leverage-residual plot and added variable plot
library(effects)
library(ggeffects)  # Load the package


# Load data from a CSV file
heart_data <- read.csv("C:/Users/35387/OneDrive - National College of Ireland/NCI data analyst/sem 2/DOMAIN/project/archive (4)/diabetes_012_health_indicators_BRFSS2015.csv", header = TRUE, stringsAsFactors = T)

# View data 
names(heart_data)
str(heart_data)

# Descriptive Statistics
summary(heart_data)

sapply(heart_data, class)

####################Data Cleaning###################################


# Identify missing values in the entire dataset
sum(is.na(heart_data))

#Identify missing values for all columns and store the result in a data frame
missing_data <- data.frame(
  Column = names(heart_data),
  Missing_Values = sapply(heart_data, function(x) sum(is.na(x))))
print(missing_data)
# Remove rows with missing values
heart_data <- na.omit(heart_data)
# Remove duplicates
heart_data <- unique(heart_data) 
summary(heart_data)


clean_data <- heart_data


####################Correlation matrix########################

sapply(clean_data, class)
summary(clean_data)

summary(clean_data)

# Identify numerical and categorical variables
numerical_vars <- sapply(clean_data, is.numeric)
categorical_vars <- setdiff(names(clean_data), names(clean_data)[numerical_vars])


# Calculate correlations for numerical variables
numerical_correlations <- cor(clean_data[, numerical_vars])
print(numerical_correlations)

#Create a heatmap
corrplot::corrplot(numerical_correlations, method = "number")
summary(clean_data)

clean_data <- clean_data[, !colnames(clean_data) %in% c("Stratification2Overall")]


summary(clean_data)

write.csv(clean_data, "C:/Users/35387/OneDrive - National College of Ireland/NCI data analyst/sem 2/DOMAIN/project/clean_data.csv", row.names = FALSE)


