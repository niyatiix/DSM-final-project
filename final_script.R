# Final Project (ID: 10493638) -------------------------------------------------------------------

install.packages("")
# clearing environment and plotting space
rm(list=ls())
dev.off()

# load libraries
library(dplyr)
library(tidyverse)
library(gridExtra)      ## for boxplot arrangement
library(reshape2)       ## for correlation plot
library(tree)
library(randomForest) 
library(ipred)          ## for bagging
library(caret)          ## for calculating variable importance

# load data
employee <- read.csv("employee_dataset.csv", sep = ";")

# simple exploration of data
ls(employee)
summary(employee)
glimpse(employee)
## data is  mix of character / integer types


# Data Cleaning -----------------------------------------------------------

## UNDERSTANDING VARIABLE OF INTEREST
# see how many of each satisfaction score
employee %>%
  group_by(Overall_SatisfactionScore) %>%
  count()

# filtering out passive responses 
employee <- employee %>%
  filter(Overall_SatisfactionScore == 'Detractor' | 
           Overall_SatisfactionScore == 'Promoter')

# change data to factors
employee_factor <- employee %>%
  mutate_if(is.character,as.factor) 
summary(employee_factor)
glimpse(employee_factor)
         
# change our variable of interest into a dummy
employee$Overall_SatisfactionScore <- 
  ifelse(employee$Overall_SatisfactionScore == "Promoter", 1, 0)

# changing gender to dummy
employee$Gender <- ifelse(employee$Gender == "Male", 1, 0)

# one hot encode department, travel type, education data
employee <- cbind(employee[, -which(names(employee) == 'Department')], 
                 model.matrix(~Department-1, employee))

employee <- cbind(employee[, -which(names(employee) == 'Traveltype_last_year')], 
                 model.matrix(~Traveltype_last_year-1, employee))

employee <- cbind(employee[, -which(names(employee) == 'EducationType')], 
                  model.matrix(~EducationType-1, employee))

# creating new ordered numerical variables for marital, education
# reviews & satisfaction scores
employee <- employee %>%
  mutate(Education = case_when(
    Education == "Under Graduation" ~ 1,
    Education == "Graduation" ~ 2,
    Education == "Masters / PHD" ~ 3,
  ),
  MaritalStatus = case_when(
    MaritalStatus == "Single" ~ 1,
    MaritalStatus == "Married" ~ 2,
    MaritalStatus == "Divorced" ~ 3,
  ),
  Potential_Review = case_when(
      PotentialReview == "Low" ~ 1,
      PotentialReview == "Medium" ~ 2,
      PotentialReview == "High" ~ 3,
      PotentialReview == "Very High" ~ 4
  ),
    Performance_Review = case_when(
      PerformanceReview == "Inconsistent" ~ 1,
      PerformanceReview == "Met Expectations" ~ 2,
      PerformanceReview == "Exceed Expectations" ~ 3
  ),
    Satisfaction_Score = case_when(
      SatisfactionScore == "Detractor" ~ 1,
      SatisfactionScore == "Passive" ~ 2,
      SatisfactionScore == "Promoter" ~ 3
  ),
    Job_Role_Satisfaction_Score = case_when(
      JobRole_SatisfactionScore == "Detractor" ~ 1,
      JobRole_SatisfactionScore == "Passive" ~ 2,
      JobRole_SatisfactionScore == "Promoter" ~ 3
    ))

# removing unnecessary variables
employee <- employee %>%
  select(-EmployeeID, -PotentialReview, -PerformanceReview, -SatisfactionScore, 
         -JobRole_SatisfactionScore)
summary(employee)
glimpse(employee)
## only dbl and int type numerical data
  
# check this has worked
sum(is.na(employee))

# moving overall satisfaction score to beginning of dataset
employee <- employee %>%
  relocate(Overall_SatisfactionScore)


# Summary Plots -----------------------------------------------

# PLOT 1 - GENDER DISTRIBUTION BY SATISFACTION SCORE
table(employee_factor$Gender)
## better to compare within genders as there are more males in the sample

g1 <- employee_factor %>% 
  group_by(Overall_SatisfactionScore, Gender) %>% 
  summarise(n=n(),'pct'=round(n/nrow(employee_factor),3)) %>%
  ungroup() %>%
  group_by(Gender) %>%
  mutate(prop = round(n/sum(n), 2)) %>%
  ggplot(aes(Overall_SatisfactionScore,n)) +
  geom_col(alpha=.7,fill='thistle3') +
  geom_point(aes(y=pct*720),size=2) +
  geom_text(aes(y=pct*650,label=prop),size=6, color = "palevioletred4") +
  theme(text=element_text(size=20)) +
  labs(title='Gender Distribution by Satisfaction Score',
       x='Gender',
       y = 'Number of employees') + 
  facet_grid(.~Gender)
plot(g1)

## slightly larger proportion of females voting detractor - more evenly spread
## slightly larger proportion of males voting promoter - bigger difference in voting

# PLOT 2 - EDUCATION FIELD & TYPE BY SATISFACTION SCORE
g2a <- employee_factor %>%
 group_by(Education, EducationType, Overall_SatisfactionScore) %>%
 summarise(n=n()) %>%
 ungroup() %>%
 group_by(Education) %>%
 mutate(prop = round(n/sum(n), 2)) %>%
 ggplot(aes(Education, prop)) +
 geom_col(alpha=.5,fill='thistle3') +
 facet_grid(EducationType~Overall_SatisfactionScore) +
 geom_text(aes(label=prop),size=4.5,color='palevioletred4') +
 labs(title ='Education Field & Type by Satisfaction Score',
      x ='Education',
      y = "Proportion of total employees")
plot(g2a)

g2b <- employee_factor %>%
 group_by(Education, Overall_SatisfactionScore) %>%
 summarise(n=n()) %>%
 ungroup() %>%
 group_by(Education) %>%
 mutate(prop = round(n/sum(n), 2)) %>%
 ggplot(aes(Education, prop)) +
 geom_col(alpha=.5,fill='thistle3') +
 facet_grid(~Overall_SatisfactionScore) +
 geom_text(aes(label=prop),size=4.5,color='palevioletred4') +
 labs(title ='Educational Level by Satisfaction Score',
      x ='Education',
      y = 'Proportion of employees')
plot(g2b)

g2c <- employee_factor %>%
 group_by(EducationType, Overall_SatisfactionScore) %>%
 summarise(n=n()) %>%
 ungroup() %>%
 group_by(EducationType) %>%
 mutate(prop = round(n/sum(n), 2)) %>%
 ggplot(aes(EducationType, prop)) +
 geom_col(alpha=.5,fill='thistle3') +
 facet_grid(~Overall_SatisfactionScore) +
 geom_text(aes(label=prop),size=4.5,color='palevioletred4') +
 labs(title ='Education Type by Satisfaction Score',
      x ='Education',
      y = 'Proportion of employees')
plot(g2c)
## most proportions for each level of satisfaction are similar / same
## doesn't seem to affect voting either promoter or detractor

#PLOT 3A - MARITAL STATUS BY SATISFACTION SCORE
g3a <- employee_factor %>%
 group_by(MaritalStatus, Overall_SatisfactionScore) %>%
 summarise(n=n(),'pct'=round(n/nrow(employee_factor),3)) %>%
 ungroup() %>%
 group_by(MaritalStatus) %>%
 mutate(prop = round(n/sum(n), 2)) %>%
 ggplot(aes(MaritalStatus, prop)) +
 geom_col(alpha=.5,fill='thistle3') +
 facet_grid(~Overall_SatisfactionScore) +
 geom_text(aes(y=prop*1.1, label=prop),size=8,color='palevioletred4') +
 theme(text=element_text(size=18),axis.text.y=element_blank(),axis.title.y=element_blank()) +
 geom_line(aes(y=prop),group=1) +
 labs(title='Marital Status Overview by Satisfaction Score',
      x='MaritalStatus')
plot(g3a)

g3b <- employee_factor %>%
 group_by(Overall_SatisfactionScore, MaritalStatus) %>%
 summarise(n=n(),'pct'=round(n/nrow(employee_factor),3)) %>%
 ungroup() %>%
 group_by(Overall_SatisfactionScore) %>%
 mutate(prop = round(n/sum(n), 2)) %>%
 ggplot(aes(Overall_SatisfactionScore, prop)) +
 geom_col(alpha=.5,fill='thistle3') +
 facet_grid(~MaritalStatus) +
 geom_text(aes(y=prop*1.11, label=prop),size=8,color='palevioletred4') +
 theme(text=element_text(size=18),axis.text.y=element_blank(),axis.title.y=element_blank()) +
 geom_line(aes(y=prop),group=1) +
 labs(title='Marital Status Overview by Satisfaction Score',
      x='Overall Satisfaction Score')
plot(g3b)

grid.arrange(g3a,g3b,nrow=2,ncol=1)
## single individuals slightly more satisfied than married

# PLOT 4 - DEPARTMENT BY SATISFACTION SCORE
g4 <- employee_factor %>%
  group_by(Department, Overall_SatisfactionScore) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  group_by(Department) %>%
  mutate(prop = round(n/sum(n), 2)) %>%
  ggplot(aes(Department, prop)) +
  geom_col(alpha=.5,fill='thistle3') +
  facet_grid(~Overall_SatisfactionScore) +
  geom_text(aes(label=prop), size = 6, color = "palevioletred4") +
  labs(title ='Department by Satisfaction Score',
       x ='Department',
       y = 'Proportion of employees')
plot(g4)
## bigger discrepancy in voting within support and client solutions

# PLOT 5 - BOXPLOT OF AGES BY SATISFACTION SCORE
g5 <-ggplot(employee_factor, aes(Overall_SatisfactionScore, Age)) +
  geom_boxplot(fill='thistle3', alpha=.5) +
  labs(title='Boxplot of Age by Overall Satisfaction Score',
       x='Overall Satisfaction Score', y='Age')

plot(g5)
## older people are slightly more likely to vote promoter

# PLOT 6 - BOXPLOT OF YEARS AT COMPANY BY OVERALL SATISFACTION SCORE
g6 <- ggplot(employee_factor, aes(Overall_SatisfactionScore, Years_at_Company)) +
  geom_boxplot(fill='thistle3', alpha=.5) +
  labs(title='Boxplot of Years at Company by Overall Satisfaction Score',
       x='Overall Satisfaction Score', y='Years at Company')
plot(g6)
## not saying much

# PLOT 7 - BOXPLOT OF SATISFACTION BY DISTANCE FROM OFFICE
g7 <- ggplot(employee_factor, aes(Overall_SatisfactionScore, DistanceToOffice)) +
  geom_boxplot(fill='thistle3', alpha=.5) +
  labs(title='Boxplot of Distance to Office by Overall Satisfaction Score',
       x='Overall Satisfaction Score', y='Distance to office')
plot(g7)

# PLOT 8 - BOXPLOT OF SATISFACTION BY MONTHLY INCOME
g8 <- ggplot(employee_factor, aes(Overall_SatisfactionScore, MonthlyIncome)) +
  geom_boxplot(fill='thistle3', alpha=.5) +
  labs(title='Boxplot of Monthly Income by Overall Satisfaction Score',
       x='Overall Satisfaction Score', y='Monthly Income')
plot(g8)
## not much difference in medians and promoter more skewed

# PLOT 9 - BOXPLOT OF SATISFACTION BY LAST SALARY HIKE
g9 <- ggplot(employee_factor, aes(Overall_SatisfactionScore, LastSalaryHike)) +
  geom_boxplot(fill='thistle3', alpha=.5) +
  labs(title='Boxplot of Last Salary Hike by Overall Satisfaction Score',
       x='Overall Satisfaction Score', y='Last Salary Hike')
plot(g9)
## detractor votes more skewed

# set up the plotting area for boxplot comparisons and plot
?gridExtra
grid.arrange(g5,g6,g8,g9, nrow=2,ncol=2)

# minor cleaning -- removing variables due to plots & nature of study
employee <- employee %>%
  select(-`EducationTypeBio-technology`, -EducationTypeEconomics, 
         -`EducationTypeMarketing / Finance`, -`EducationTypePhycology / Behavior Sciences`, 
         -MaritalStatus, -Education)

employee_factor <- employee_factor %>%
  select(-EmployeeID, -Education, -EducationType, -MaritalStatus)

# clear up environment
rm(g1, g2a, g2b, g2c, g3a, g3b,  g4, g5, g6, g7, g8, g9)


# Correlation Matrix ------------------------------------------------------

# correlation matrix using newly cleaned numeric data
corr_mat <- cor(employee)
# convert matrix to long format
corr_matrix_melt <- melt(corr_mat)
?geom_tile
# plot correlation matrix
ggplot(corr_matrix_melt, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low="turquoise4", mid="white", high="palevioletred4", midpoint=0) +
  theme_minimal() +
  labs(title = "Correlation Plot of Employee data", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

corr_tibble <- as_tibble(corr_mat)


# Exploration - PCA -------------------------------------------------------------

# resetting grid space
dev.off()
par(mfrow = c(1, 1))

# important to scale variables so all have variance = 1 (TRUE)
pca.out <- prcomp(employee, center = TRUE, scale. = TRUE)
names(pca.out)
# get all principal component loadings
pca.out$rotation
# get PC loadings for first 5 PCs
pca.out$rotation[,1:5]
# create vector which orders loadings of first and second principal component
ordered_pc1 <- pca.out$rotation[,1][order(pca.out$rotation[,1])]
view(rev(ordered_pc1))
ordered_pc2 <- pca.out$rotation[,2][order(pca.out$rotation[,2])]
view(rev(ordered_pc2))
dim(pca.out$x)

# plot loading vectors of first 2 PCs
biplot(pca.out, scale = 0)

# axes to face a different direction for easier interpretation
pca.out$rotation <- -pca.out$rotation
pca.out$x <- -pca.out$x
biplot(pca.out, scale=0)
## still difficult to see clearly....

# plot scree plot -- 
# checking the standard deviations of the PCs
pca.out$sdev
# the variance explained by each PC is the squared standard deviation
pca.var <- pca.out$sdev^2
pca.var
# proportion of variance explained
pve <- pca.var / sum(pca.var)
pve

# plotting PVE and cumulative PVE by component
par(mfrow = c(1, 2))
plot(pve, 
     xlab="Principal Component", 
     ylab="Proportion of Variance Explained", 
     ylim=c(0,1),
     type='b')
plot(cumsum(pve),
     xlab="Principal Component", 
     ylab="Cumulative Proportion of Variance Explained", 
     ylim=c(0,1),
     type='b')

# difficult to identify an elbow - maybe slight after 3PCs
# cumulative graph is showing we need more PCs to explain the data 


# Exploration - Hierarchical Clustering  ----------------
dev.off()
# scale the numeric dataframe
employee_scaled <- scale(employee)

# clustering observations -- ignore

# # hierarchical clustering on observations using complete linkage
# hc.complete <- hclust(dist(employee_scaled), method="complete")
# # hierarchical clustering on observations using average linkage
# hc.average <- hclust(dist(employee_scaled), method="average")
# # hierarchical clustering on observations using single linkage
# hc.single <- hclust(dist(employee_scaled), method="single")
# 
# # create view of the 3 plots 
# par(mfrow=c(1,3))
# plot(hc.complete, main="Complete Linkage", xlab="", sub="", cex =.9)
# plot(hc.average, main="Average Linkage", xlab="", sub="", cex =.9)
# plot(hc.single, main="Single Linkage", xlab="", sub="", cex =.9)
# 
# # cut complete dendogram into 4 clusters and assign the observations
# cutree(hc.complete, 4)
# #abline(h = 0.95, col = 'violet')
# # cut average dendogram into 8
# y <- cutree(hc.complete, 8)
# plot(y)

# hierarchical clustering using a correlation-based dissimilarity matrix 
# instead of the default Euclidean distance measure as above
?dist

# create dissimilarity matrix using correlation matrix from before
diss_mat <- as.matrix(1-abs(corr_mat))
par(mfrow = c(1,3))

hc.complete.corr <- hclust(as.dist(diss_mat), method = "complete")
plot(hc.complete.corr, main = "Hierarchical Clustering with correlation - Scaled Complete")

hc.average.corr <- hclust(as.dist(diss_mat), method = "average")
plot(hc.average.corr, main = "Hierarchical Clustering with correlation - Scaled Average")

hc.single.corr <- hclust(as.dist(diss_mat), method = "single")
plot(hc.single.corr, main = "Hierarchical Clustering with correlation - Scaled Single")
  
# outputs of clusters of similar variables (6 clusters)
cluster_complete <- cutree(hc.complete.corr, k = 6)
view(cluster_complete)
cluster_average <- cutree(hc.average.corr, k = 6)
view(cluster_average)
cluster_single <- cutree(hc.single.corr, k = 6)
view(cluster_single)

# plot threshold to identify clusters FROM XXX LINKAGE (6)
par(mfrow = c(1,1))
plot(hc.complete.corr, labels = colnames(diss_mat))
abline(h = 0.982, col = 'violet')

# a <- cutree(hc9, k = 5)
# b <- cutree(hc10, k = 5)
# c <- cutree(hc11, k = 5)

# tb <- table(c(a,b,c, colnames(dissimilarity_matrix)))
# view(tb)
# 
# par(mfrow = c(1,1))
# plot(hc9, labels = colnames(dissimilarity_matrix))
# abline(h = 0.95, col = 'palevioletred4')

# Inference ---------------------------------------------------------------

# removing variables from exploration part
# employee <- employee %>%
#   select(-)
# 
# employee_factor <- employee_factor %>%
#   select(-)

# fit a decision tree explaining the overall employee satisfaction score 
# variables with all other variables in the employee dataset

# set seed for reproducibility within resampling
set.seed(5432)
# 80% of data randomly sampled into training
train_index <- sample(nrow(employee_factor), 0.8 * nrow(employee_factor))
# creating train & test subset 
train <- employee_factor[train_index, ]
test <- employee_factor[-train_index, ]

# decision tree
tree_model <- tree(Overall_SatisfactionScore ~ ., data = train)
tree_model2 <- tree(Overall_SatisfactionScore ~ ., data = employee_factor)
# decision tree summary & plot
summary(tree_model)
tree_model
plot(tree_model)
text(tree_model, pretty = 0)

summary(tree_model2)
plot(tree_model2)
text(tree_model2, pretty = 0)

# let's prune the tree with the same approach as earlier
cv.employee <- cv.tree(tree_model)
plot(cv.employee$size, cv.employee$dev, type='b')
# identify the size of the best fitting tree
best.employee <- cv.employee$size[cv.employee$dev==min(cv.employee$dev)]

# prune tree to optimal size (note: it may be the same as the original tree)
prune.employee <- prune.tree(tree_model, best=best.employee)
prune.employee
# plot the pruned tree
plot(prune.employee)
text(prune.employee)



# random forest
?randomForest
rf_model <- randomForest(Overall_SatisfactionScore ~ ., 
                         data = train, 
                         ntree = 300,
                         importance = TRUE)
rf_model
rf_model2 <- randomForest(Overall_SatisfactionScore ~ ., 
                         data = employee_factor, 
                         ntree = 300,
                         importance = TRUE)
rf <- randomForest(factor(Overall_SatisfactionScore)~.,
                   data = employee_factor,
                   ntree=500,
                   type='classification')
rf
view(importance(rf))
plot(rf)
# random forest summary & plot
rf_importance <- importance(rf_model)
view(rf_importance)
rf_model
plot(rf_model)
rf_importance2 <- importance(rf_model2)
view(rf_importance2)
plot(rf_model2)


# bagging
?bagging

bag_employee <- bagging(Overall_SatisfactionScore ~ ., 
                            data = train, 
                            nbagg = 200,
                            coob = TRUE)
bag_employee

# visualize the importance of the predictor variables 
# by calculating the total reduction in RSS
#calculate variable importance
VI <- data.frame(var=names(train[,-1]), imp=varImp(bag_employee))

#sort variable importance descending
VI_plot <- VI[order(VI$Overall, decreasing=TRUE),]
print(VI_plot)

#visualize variable importance with horizontal bar plot
barplot(VI_plot$Overall,
        names.arg=rownames(VI_plot),
        horiz=TRUE,
        col='palevioletred4',
        xlab='Variable Importance')

# https://www.statology.org/bagging-in-r/

# using model to make predictions




