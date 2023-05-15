install.packages("")

# load libraries
library(dplyr)
library(stats)
library(tree)
library(randomForest)
library(gbm)

# load dataset in
employee <- read.csv("employee_dataset.csv", sep = ";")
ls(employee)

employee$Gender <- as.factor(employee$Gender)
employee$Education <- as.factor(employee$Education)
employee$EducationType <- as.factor(employee$EducationType)
employee$MaritalStatus <- as.factor(employee$MaritalStatus)
employee$Department <- as.factor(employee$Department)
employee$Traveltype_last_year <- as.factor(employee$Traveltype_last_year)
summary(employee)

# creating new ordered numerical variables for reviews & satisfaction scores
employee <- employee %>%
  mutate(potential_review = case_when(
    PotentialReview == "Low" ~ 1,
    PotentialReview == "Medium" ~ 2,
    PotentialReview == "High" ~ 3,
    PotentialReview == "Very High" ~ 4
  ),
  performance_review = case_when(
    PerformanceReview == "Inconsistent" ~ 1,
    PerformanceReview == "Met Expectations" ~ 2,
    PerformanceReview == "Exceed Expectations" ~ 3
  ),
  satisfaction_score = case_when(
    SatisfactionScore == "Detractor" ~ 1,
    SatisfactionScore == "Passive" ~ 2,
    SatisfactionScore == "Promoter" ~ 3
  ),
  job_role_satisfaction_score = case_when(
    JobRole_SatisfactionScore == "Detractor" ~ 1,
    JobRole_SatisfactionScore == "Passive" ~ 2,
    JobRole_SatisfactionScore == "Promoter" ~ 3
  ),
  overall_satisfaction = case_when(
    Overall_SatisfactionScore == "Detractor" ~ 1,
    Overall_SatisfactionScore == "Passive" ~ 2,
    Overall_SatisfactionScore == "Promoter" ~ 3
  ))


employee <- employee %>%
  select(-EmployeeID, -PotentialReview, -PerformanceReview, -SatisfactionScore, 
         -JobRole_SatisfactionScore,-Overall_SatisfactionScore)

summary(employee)

# PCA
y = as.matrix(employee[,20])
x = as.matrix(employee[,1:19])
pca <- prcomp(x, employee, scale = TRUE)
summary(pca)
plot(pca)

