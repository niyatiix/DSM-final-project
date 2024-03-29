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

# HIERARCHICAL 

# cluster observations using complete linkage
hc.complete <- hclust(dist(employee), method="complete")
# hierarchical clustering with average linkage
hc.average <- hclust(dist(employee), method="average")
# HC with single linkage
hc.single <- hclust(dist(employee), method="single")

# create view of the 3 plots 
par(mfrow=c(1,3))
plot(hc.complete,
     main="Complete Linkage", 
     xlab="", 
     sub="",
     cex =.9)
plot(hc.average, 
     main="Average Linkage", 
     xlab="", 
     sub="",
     cex =.9)
plot(hc.single , 
     main="Single Linkage", 
     xlab="", 
     sub="",
     cex =.9)
# cut complete dendogram into 4 clusters and assign the observations
cutree(hc.complete, 4)
abline(h = 0.95, col = 'violet')

# scale the numeric dataframe
employee_scaled <- scale(employee)

# hierarchical clustering on scaled data using complete linkage
hc4 <- hclust(dist(employee_scaled), method = "complete")
plot(hc4, main = "Hierarchical Clustering (scaled) - Complete")
# hierarchical clustering on scaled data using average linkage
hc5 <-hclust(dist(employee_scaled), method = "average")
plot(hc5)

# cut dendogram into 8 clusters
y <- cutree(hc4, 8)
# plot cluster assignments of observations
plot(y)

# using correlation as a distance measure instead of euclidean distance
?dist
correlation_matrix <- as.matrix(cor(employee, method = "pearson"))

dissimilarity_matrix <- as.matrix(1-abs(correlation_matrix))
par(mfrow = c(1,3))

hc6 <- hclust(as.dist(dissimilarity_matrix), method = "complete")
plot(hc6, main = "Hierarchical Clustering with correlation - Scaled Complete")

hc7 <- hclust(as.dist(dissimilarity_matrix), method = "single")
plot(hc7, main = "Hierarchical Clustering with correlation - Scaled Single")

hc8 <- hclust(as.dist(dissimilarity_matrix), method = "average")
plot(hc8, main = "Hierarchical Clustering with correlation - Scaled Average")

# outputs of clusters
cutree(hc6, k = 5)
cutree(hc7, k = 5)
cutree(hc8, k = 5)


# REPLACING FOR NO MODULIUS
dissimilarity_matrix_2 <- as.matrix(1-correlation_matrix)

hc9 <- hclust(as.dist(dissimilarity_matrix_2), method = "complete")
plot(hc6, main = "Hierarchical Clustering with correlation - Scaled Complete")

hc10 <- hclust(as.dist(dissimilarity_matrix_2), method = "single")
plot(hc7, main = "Hierarchical Clustering with correlation - Scaled Single")

hc11 <- hclust(as.dist(dissimilarity_matrix_2), method = "average")
plot(hc8, main = "Hierarchical Clustering with correlation - Scaled Average")

a <- cutree(hc9, k = 5)
b <- cutree(hc10, k = 5)
c <- cutree(hc11, k = 5)

tb <- table(c(a,b,c, colnames(dissimilarity_matrix)))
view(tb)

par(mfrow = c(1,1))
plot(hc9, labels = colnames(dissimilarity_matrix))
abline(h = 0.95, col = 'paleredviolet4')


features <- c("Overall_SatisfactionScore", "Age", "BillingRate", "DistanceToOffice",, "Gender",
              "LastSalaryHike", "MaritalStatus", "MonthlyIncome",
              "PerformanceReview", "PotentialReview", "TotalCompanies",
              "TotalExperience", "Traveltype_last_year", "Years_at_Company",
              "Years_InCurrentRole", "Overall_SatisfactionScore")



