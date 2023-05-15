
# Final Report File -------------------------------------------------------
rm()

getwd()
setwd("/Users/harry/Desktop/Data Science Modelling/Final Project")
## ISLR / LEAPS/ glmnet needed

?rlang
remove.packages("rlang")

# Installing Packages  ----------------------------------------------------
library(dplyr)
library(tidyverse)
library(readr) 
library(haven) 
library("corrplot")
library(ISLR)
library(leaps)
library(glmnet)
library(tree) 
library(randomForest) 
library(gbm)
#install.packages('MASS') 
library(MASS)

?read_csv
?read.csv

data <- read.csv('employee_dataset.csv', sep = ';')


# Getting to know the data ------------------------------------------------
glimpse(data)
head(data)
summary(data)


# Only numeric Cor Plot

numeric <-select_if(data, is.numeric)
correlation <- cor(numeric)


# Character, the ordering method of the correlation matrix.
# 'FPC' for the first principal component order.

corrplot(correlation, col = 'black')

# seeing how happy and unhappy fit into the corrplot:
corrplot(correlation, method= 'ellipse', col = 'black')

# Sorting Categorical Data ------------------------------------------------
# FACTORING THE DATA IN ONE LINE!!
## stored as character not factor so redoing this step
data <- as.data.frame(unclass(data),                     # Convert all columns to factor
                      stringsAsFactors = TRUE)
glimpse(data)
summary(data)

### Now have numeric versions of all variables, which we can maybe do a corr plot for 
# include this to output a table with all correlation values
numeric <-select_if(data, is.numeric)
correlation <- cor(numeric)
print(correlation)

# Creating a tag variable  ------------------------------------------------
## Numeric variables With Key

data <- data %>% 
  mutate(happy = ifelse(Overall_SatisfactionScore == 'Promoter', 2,
                   ifelse(Overall_SatisfactionScore == 'Passive', 1, 0)))
glimpse(data)

## Doing the same for the other satisfaction scores 
data <- data %>% 
  mutate(satisfaction_2 = ifelse(SatisfactionScore == 'Promoter', 2 , 
                                 ifelse(SatisfactionScore == 'Passive', 1, 0)))

### Job Satisfaction 
data <- data %>% 
  mutate(Job_satisfaction_2 = ifelse(JobRole_SatisfactionScore == 'Promoter', 2,
                                     ifelse(JobRole_SatisfactionScore == 'Passive', 1, 0)))

glimpse(data)
glimpse(categorical)

### Gender 
unique(data$Gender)

data <- data %>% 
  mutate(Gender_ = ifelse(Gender == "Male", 1, 0))

#### Education 
unique(data$Education)

data <- data %>% 
        mutate(Education_ = ifelse(Education == "Under Graduation", 0, 
                                   ifelse(Education == "Graduation", 1, 2)))
### Education Type:
unique(data$EducationType)

data <- data %>% 
  mutate(Education_Type_ = ifelse(EducationType == "Economics", 0, 
                              ifelse(EducationType == "Marketing / Finance", 1,
                                  ifelse(EducationType == "Bio-technology", 2, 3))))
### Martial Status 
unique(data$MaritalStatus)

data <- data %>% 
        mutate(MaritalStatus_ = ifelse(MaritalStatus == "Single", 0, 
                                    ifelse(MaritalStatus == "Married", 1, 2)))

### Department Need to rerun as overrode the previous name 
unique(data$Department)
                                       
data <- data %>% 
        mutate(Department_ = ifelse(Department == "ClientSolutions", 0, 
                                 ifelse(Department == "BusinessDevelopment", 1, 2)))

##travel type_last_year
unique(data$Traveltype_last_year)

data <- data %>% 
  mutate(Traveltype_last_year_ = ifelse(Traveltype_last_year == "Conference", 0, 
                                    ifelse(Traveltype_last_year == "LongTermProject", 1,
                                        ifelse(Traveltype_last_year == "No", 2, 
                                            ifelse(Traveltype_last_year == "ClientEngagement", 3, 4)))))

## Potential Review 
unique(data$PotentialReview)

data <- data %>% 
  mutate(PotentialReview_ = ifelse(PotentialReview == "Low", 0, 
                                ifelse(PotentialReview == "Medium", 1,
                                    ifelse(PotentialReview == "High", 2, 3))))

### Performance Review:
unique(data$PerformanceReview)

data <- data %>% 
        mutate(PotentialReview_ = ifelse(PerformanceReview == "Inconsistent", 0, 
                                      ifelse(PerformanceReview == "Met Expectations", 1, 2)))

summary(data)
glimpse(data)

# Final Data Cleaned and Numerated ----------------------------------------
# ONLY USES DATA WHICH IS NUMERIC!
data_for_analysis <- data %>% 
                     select_if(is.numeric)

summary(data_for_analysis)
glimpse(data_for_analysis)

# Unsupervised Learning Method --------------------------------------------
?hclust 
?cex


#### We can't use k-Means or PCA because we have categorical data. As a result, we need to use Hierarchical 
# plotting HCs which uses euclidean distance between each observation in predictor space
hc.first <- hclust(dist(data_for_analysis), method = "complete" )

hc.second <- hclust(dist(data_for_analysis), method = "single" )

hc.third <- hclust(dist(data_for_analysis), method = "average" )

par(mfrow = c(1, 3))

plot (hc.first, main = " Complete Linkage ",
      xlab = "", sub = "", cex = 0.2)

plot (hc.second , main = " Average Linkage ",
        xlab = "", sub = "", cex = 0.2)

plot (hc.third, main = " Single Linkage ",
      xlab = "", sub = "", cex = 0.2)

x <- cutree(hc.first, 8)

## Checking the scale of Variables before doing PCA


# Better Method with scaling ----------------------------------------------
# complete and average plotted
data_scaled <- scale (data_for_analysis)
    
right <-hclust(dist(data_scaled), method = "complete")

par(mfrow = c(1,1))

plot(right, main = "Heirarchical scaled", cex = 0.000001)

right_ <-hclust(dist(data_scaled), method = "average")
plot(right_)

y <- cutree(right, 8)
plot(y)

# Using correlation as a distance measure  --------------------------------
?hclust 
?dist

?as.dist

#right <-hclust(dist(data_scaled,, method = "complete")

cor(data_for_analysis, method= "pearson")

correlation_matrix <- as.matrix(cor(data_for_analysis, method= "pearson"))


# Correlation as Distance Measure  ----------------------------------------

dissimilarity_matrix <- as.matrix(1-abs(correlation_matrix))

par(mfrow = c(1,3))

lice <- hclust(as.dist(dissimilarity_matrix), method = "complete")
plot(lice, main = "Heirarchical scaled with correlation comp", cex = 0.000001)

lice_2 <- hclust(as.dist(dissimilarity_matrix), method = "single")
plot(lice_2, main = "Heirarchical scaled with correlation si", cex = 0.000001)

lice_3 <- hclust(as.dist(dissimilarity_matrix), method = "average")
plot(lice_3, main = "Heirarchical scaled with correlation avg", cex = 0.000001)


# Outputs of Clusters -----------------------------------------------------
cutree(lice, k= 5)

cutree(lice_2, k= 5)

cutree(lice_3, k= 5)



# Replicating for no modulius ---------------------------------------------
dissimilarity_matrix_2 <- as.matrix(1-correlation_matrix)

par(mfrow = c(1,3))

lice <- hclust(as.dist(dissimilarity_matrix_2), method = "complete")
plot(lice, main = "Heirarchical scaled with correlation comp", cex = 0.000001)

lice_2 <- hclust(as.dist(dissimilarity_matrix_2), method = "single")
plot(lice_2, main = "Heirarchical scaled with correlation si", cex = 0.000001)

lice_3 <- hclust(as.dist(dissimilarity_matrix_2), method = "average")
plot(lice_3, main = "Heirarchical scaled with correlation avg", cex = 0.000001)


# Outputs of Clusters -----------------------------------------------------

x <- cutree(lice, k= 5)

y <- cutree(lice_2, k= 5)

z <- cutree(lice_3, k= 5)

tb <- table(c(x,y,z, colnames(dissimilarity_matrix)))
view(tb)

par (mfrow = c(1, 1))
plot (lice , labels = colnames(dissimilarity_matrix))
abline (h =0.55, col = " red ")



apply(data_for_analysis, 2, mean) # this applies the mean function to each column
apply(data_for_analysis, 2, var)  # this applies the var functions to each column
# these show that the variables are not quite comparable - we should scale them before PCA!
## Cannot do with

pr.out <- prcomp(data_for_analysis, scale=TRUE)
names(pr.out)   # parts of the result object
pr.out$center   # means used for the scaling
pr.out$scale    # standard deviations used for the scaling (should be sqrt(vars) from above)
pr.out$rotation # PC loading vectors - there are 4 PCs
dim(pr.out$x)   # PC scores

summary(pr.out)




apply(PCA_data, 2, mean) # this applies the mean function to each column
## needs scaling
apply(PCA_data, 2, var)  # this applies the var functions to each column

pr.out <- prcomp(PCA_data, scale=TRUE)
names(pr.out)   # parts of the result object
pr.out$center   # means used for the scaling
pr.out$scale    # standard deviations used for the scaling (should be sqrt(vars) from above)
pr.out$rotation # PC loading vectors - there are 4 PCs
dim(pr.out$x)   # PC scores

biplot(pr.out, scale=0)



# we can also check the standard deviations of the PCs
pr.out$sdev
# the variance explained by each PC is the squared standard deviation
pr.var <- pr.out$sdev^2
pr.var
# and the proportion of variance explained is easily calculated like this
pve <- pr.var / sum(pr.var)
pve




# K-Means Clustering ------------------------------------------------------
km <- kmeans(data[,-1], 2, nstart = 20)
km


# apply k-means clustering with 2 clusters and 20 different starting values
km.out <- kmeans(x, 2, nstart=20)
km.out$cluster # cluster memberships (not surprisingly a 25-25 split)
# scatterplot of the data with with observations coloured by cluster membership
plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results with K=2", xlab="", ylab="", pch=20, cex=2)

# now, something less trivial: we apply k-means looking for 3 clusters
km.out <- kmeans(x, 3, nstart=20)
km.out # different parts of the result object
# scatterplot with cluster memberships
plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results with K=3", xlab="", ylab="", pch=20, cex=2)

# we can easily see how much multiple starting values matter
km.out <- kmeans(x,3,nstart=1)  # start from one random partition
km.out$tot.withinss             # amount of variation within clusters
km.out <- kmeans(x,3,nstart=20) # start from 20 random partitions and choose the best
km.out$tot.withinss             # amoung of variation within clusters (should be lower then above)

#### not working 


# Creating Table of INDEX -------------------------------------------------

































#### what is col linear- use Principal component analysis here
happy <- data %>% 
lda_happy <- lda(happy ~., data = data)

# Storing as factor -------------------------------------------------------
character <- select_if(data, is.character)
# 12 variables character, which we can't dp analysis with      
## turning into factors
data <- as.data.frame(unclass(data),                     # Convert all columns to factor
                      stringsAsFactors = TRUE)
glimpse(data) # Only factors now
unique(data$Overall_SatisfactionScore)

?ifelse





