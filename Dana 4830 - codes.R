# DANA 4830 codes ####

## Jan 16 - PCA Analysis#### 

attach(MovieSample)
head(MovieSample)
df <- MovieSample[,2:5]

#1. Compute the mean and variances for each of the variables in the data set.
means <- c(mean(Movie1_Toy.Story),mean(Movie2_MonstersInc.), mean(Movie3_Saw),mean(Movie4_Ring))
means
vars <- c(var(Movie1_Toy.Story),var(Movie2_MonstersInc.), var(Movie3_Saw),var(Movie4_Ring))
vars

#2. Obtain a correlation matrix for the variables.
cor(df)

#3. Draw scatterplots comparing variables (You can do one or two).
pairs(df)

#4. Center each variable and obtain the variances for the centered data.
df_center <- scale(df, center = TRUE, scale = FALSE)
df_center_scale <- scale(df, center = TRUE, scale = TRUE) # not needed, just experimenting.

var(df_center)
var(df_center_scale) # not needed, just experimenting.

#5. Calculate the covariance matrix.
cov_center <- cov(df_center_scale)
cov_center

#6. Calculate the eigenvalues of the covariance matrix.
e <- eigen(cov_center)
e
e$values

#7. Calculate the eigenvectors of the covariance matrix.
e$vectors

#8. Order the eigenvectors.
egvc <- e$vectors

#9. Calculate the principal components.
PC <- round(as.matrix(df_center_scale)%*% egvc, 3)
PC
plot(PC[,1], PC[,2])


#10. Use the prcomp() function in R to obtain the principal components.
PCA <- prcomp(df)
PCA
PCA$sdev^2 #Eigenvalues
PCA$rotation # Eigenvectors
PCA$x # Principal Components

#11. Based on your results, how many principal components should we take?
cumsum(PCA$sdev^2)
6.44/6.6
#I would take the first 2 PCs because together they hold about 97% of the variance.















## Jan 23 - PCA / PC selection / Varimax ####

### Case 1 ####
#Read the MovieSample.csv data into R. The data consists of 4 variables for 5 users:
attach(MovieSample)
head(MovieSample)

#1. Perform PCA in R on the data set using standardized data.
df <- scale(MovieSample[,2:5], center = TRUE, scale = TRUE)
df
PCA <- prcomp(df)
PCA$x

#2. Obtain the eigenvalues and eigenvectors.

## method 1
cov_matrix <- cov(df)
e <- eigen(cov_matrix)
e$values
e$vectors

## method 2
PCA$sdev^2 #Eigenvalues
PCA$rotation # Eigenvectors


#3. Use the Kaiser Criterion, Total variation and Screeplot to determine the number of PCs to keep.

### Kaiser
mean(e$values) # because the avrg of the eigenvalues is 1, and only eigenvalues for PC1 and PC2 are > 1, we keep those.

### Total Variation
total_var <- 2.27879025 + 1.59246246 + 0.10974045 + 0.01900684
var1 <- 2.27879025/total_var # this PC contains 57% of the variance
var2 <- 1.59246246/total_var # this PC contains 40% of the variance. Because it adds to 97%, this is enough.

#### another method
cumsum((PCA$sdev^2)/4)

### Screeplot
#### method 1
screeplot(PCA, main ="Scree Plot", xlab="Components")
screeplot(PCA, main="Scree Plot", type="line")
#### method 2
library("factoextra")
fviz_eig(PCA)

#4. Obtain the eigenvector and varimax rotation loadings.

# loadings w/o rotation
cbind(sqrt(e$values[1])*e$vectors[,1],
      sqrt(e$values[2])*e$vectors[,2],
      sqrt(e$values[3])*e$vectors[,3],
      sqrt(e$values[4])*e$vectors[,4])

# Varimax rotation
varimax_rotation <- varimax(PCA$rotation)
varimax_rotation


library(psych)
fit <- principal(df, nfactors=4, rotate="varimax")
fit # print results

#5. Interpret the loadings
# Movie3 and Movie4 are more loaded to PC1 while Movie1 and Movie2 are more loaded to PC2.





### Case 2 #### 
#Read the wine.csv data into R:
attach(wine)
head(wine)

#1. Subset the first 6 variables in the dataset. (View data to check variables)
df <- wine[2:7]
head(df,10)

#2. Perform PCA in R on the data set using standardized data.
df_standard <- scale(df, center = TRUE, scale = TRUE)
head(df_standard)

cor(df)
cov(df_standard)

#3. Obtain the eigen values and eigen vectors.
PCA <- prcomp(df_standard)
PCA
PCA$sdev^2 #Eigenvalues
cumsum(PCA$sdev^2)
PCA$rotation # Eigenvectors
PCA$x # Principal Components

#4. Use the Kaiser Criterion, Total variation and Screeplot to determine the number of PCs to keep.

### Kaiser
PCA$sdev^2 #Eigenvalues
# because the avrg of the eigenvalues is 1, and only eigenvalues for PC1, PC2 and PC3 are > 1, we keep those.

### Total Variation: sum of PCs that together add 80%-90% of the total variance.
PCA$sdev^2 #Eigenvalues
cum_sum <- cumsum(PCA$sdev^2)
cum_sum/6
# In this case, four PCs would be taken. 

#Screeplot
library("factoextra")
(PCA$sdev^2)/6 #Eigenvalues / number of PCs. This is the % if explained variances.
fviz_eig(PCA)
# in this case, I would take the first 3. 

#5. Obtain the eigenvector and varimax rotation loadings.
library(psych)
fit <- principal(df_standard, nfactors=3, rotate="varimax")
fit2 <- principal(df_standard, nfactors=3, rotate = "none")

fit$loadings 
fit2$loadings 


#6. Interpret the loadings.
# After varimax rotation, the loadings represent the correlations between the variables and the Varimax rotated scores.
# Alcohol correlates a lot to RC1. 
# Malic.Acid correlates a lot to RC2.
# Mg does not correlate to any RC.


### Case 2 with PRINCOMP ####
pcal <- princomp(df, scores = TRUE, cor = TRUE) # run analysis
summary(pcal) # see results
plot(pcal) # see screeplot
screeplot(pcal,type="line",main="Screen Plot") # another screeplot
pcal$loadings














## Jan 30 - factor analysis ####

#Read the wine.csv data into R:
attach(wine)

#1. View data to check variables
head(wine)
str(wine)
df <- wine[2:14]

#2. Perform EFA in R using the fa() function for 3 and 4 factors. (Choose your preferred rotation)
library(psych)
EFA_model <- fa(df)

library(GPArotation)
EFA_model_2 <- fa(df, nfactors = 2, rotate = "oblimin")

EFA_model_3 <- fa(df, nfactors = 3, rotate = "oblimin")

EFA_model_4 <- fa(df, nfactors = 4, rotate = "oblimin")


#3. Plot the factor model for each case.
fa.diagram(EFA_model)# 1 factor.
fa.diagram(EFA_model_2)# 2 factors.
fa.diagram(EFA_model_3)# 3 factors.
fa.diagram(EFA_model_4)# 4 factors.

#4. Compare the Cumulative Variation and report on the amount of variation captured by the factors.
EFA_model_3$loadings #Cumulative Var 0.308 0.471 0.573
EFA_model_4$loadings #Cumulative Var 0.245 0.396 0.508 0.615
  
#5. Test the data for correlations using the Bartlett’s test and interpret the results.
correlations = cor(df)
correlations
cortest.bartlett(correlations, n = nrow(df)) # with a p-value of close to 0, we reject H0: No correlation structure 

#6. Test Sampling adequacy using KMO test and interpret the results.
KMO(correlations)
##Overall MSA = 0.78 meaning that there are a significant number of factors in the dataset.
nofactors = fa.parallel(wine, fm="ml", fa="fa") # we should go with 3 factors













## Feb 8 - Midterm preparation ####
attach(SBP_DBP)
df <- scale(SBP_DBP) # scaling data
head(df)

# using PRINCOMP()
PCA <- princomp(df)
PCA
PCA$sdev^2 # Eigenvalues
PCA$loadings # Eigenvectors
PCA$scores # Principal Components
# ----------------------------------- #
mean(PCA$sdev^2) # based on Kaiser criterion I would use PC1 and PC2
cumsum(PCA$sdev^2)

# using PRCOMP()
PCA2 <- prcomp(df)
PCA2$sdev^2 # Eigenvalues
PCA2$rotation # Eigenvectors
PCA2$x # Principal Components
#-----------------------------------#
mean(PCA2$sdev^2) # based on Kaiser criterion I would use PC1 and PC2
cumsum(PCA2$sdev^2/4) # based on Total Variance, I would use PC1 and PC2 as well
#-----------------------------------#
library(psych)
fit <- principal(df, nfactors=2, rotate="none") # loadings
fit$loadings
fit$scores # see the rotated RC1 and RC2 results
#-----------------------------------#
fit2 <- principal(df, nfactors=2, rotate="Varimax") # Varimax rotation
fit2$loadings
fit2$scores # see the rotated RC1 and RC2 results. 













## Feb 27 - LDA ####

#1. Read the algerian_forest_train.csv and algerian_forest_test.csv file into R.
train <- algerian_forest_train
test <- algerian_forest_test

#2. Check to see if both data sets have the same variable names.
names(train)
names(test)
names(train) == names(test)

#3. View first few rows of both data sets.
head(train)
head(test)

#4. Compute summary statistics for the training and test data
summary(train)
summary(test)

nrow(train[train$Classes == 'fire',])
nrow(train[train$Classes == 'not fire',])

nrow(test[test$Classes == 'fire',])
nrow(test[test$Classes == 'not fire   ',])

test$Classes[test$Classes == 'not fire   '] <- 'not fire'
test
nrow(test[test$Classes == 'not fire',])


#5. Use the first two variables Temp and RH to run LDA on the training data. Consider fires as a positive result.
library(MASS)
lda <- lda(Classes ~ Temperature + RH, data = train)
lda

#6. Do predictions on the test data using predict(fitted LDA mode, na.roughfix(test_data))
predictions <- predict(lda, newdata = test)
predictions
summary(predictions$class)

#7. Plot the ROC curve for the analysis.
# cross-table
cross_table <- table(predictions$class, test$Classes)
cross_table

# confusion matrix
library(caret)
caret::confusionMatrix(cross_table, positive = "fire")

# ROC curve
probs <- as.data.frame(predictions$posterior)
probs # the probability of the outcome from our LDA model

pred.LDA <- data.frame(test$Classes, probs$fire)
colnames(pred.LDA) <- c("target_label","prob_fire")
pred.LDA # the label in our test data, and the prob of outcome = 1 = fire. Like the cross_table but expanded.

labels <- as.factor(ifelse(pred.LDA$target_label=="fire", 1, 0))
labels
predictions2 <- pred.LDA$prob_fire
predictions2

library(AUC)
auc(roc(predictions2, labels), min = 0, max = 1)
plot(roc(predictions2, labels), min=0, max=1, type="l", main="LDA - ROC Chart")


#8. Use the all variables to run LDA on the training data. Consider fires as a positive result.
lda2 <- lda(Classes ~ ., data = train)
lda2

#9. Do predictions on the test data using predict(fitted LDA mode, na.roughfix(test_data)).
predictions2 <- predict(lda2, newdata = test)
predictions2
summary(predictions2$class)

#10. Plot the ROC curve for the analysis.
# cross-table
cross_table2 <- table(predictions2$class, test$Classes)
cross_table2

# confusion matrix
library(caret)
caret::confusionMatrix(cross_table2, positive = "fire")

# ROC curve
probs2 <- as.data.frame(predictions2$posterior)
probs2 # the probability of the outcome from our LDA model

pred.LDA2 <- data.frame(test$Classes, probs2$fire)
colnames(pred.LDA2) <- c("target_label","prob_fire")
pred.LDA2 # the label in our test data, and the prob of outcome = 1 = fire. Like the cross_table but expanded.

labels2 <- as.factor(ifelse(pred.LDA2$target_label=="fire", 1, 0))
labels2
predictions22 <- pred.LDA2$prob_fire
predictions22

library(AUC)
auc(roc(predictions22, labels2), min = 0, max = 1)
plot(roc(predictions22, labels2), min=0, max=1, type="l", main="LDA - ROC Chart")






















## Mar 1 - Stepwise Regression #### 

# Task 1: Describe the dataset. 
attach(toyota_corolla)
head(toyota_corolla)
tail(toyota_corolla)
summary(toyota_corolla)
dim(toyota_corolla)
str(toyota_corolla)
unique(toyota_corolla$Fuel_Type)
View(toyota_corolla)
sum(is.na(toyota_corolla))
table(is.na(toyota_corolla))
#na.omit(toyota_corolla) to omit NA values

#Task 2: Partition the dataset into 2: training (60%) and validation (40%).
train <- toyota_corolla[c(0:862),]
train
test <- toyota_corolla[c(863:1436),]
tail(test)

# Task 3: Run multiple linear regression model price (the outcome variable) and the other variables (as predictors) using only the training set.
model <- lm(Price ~ . , data = train)
summary(model)

# Task 4: Using the estimated model to predict prices for 20 cars in the validation set.
predictions <- predict(model, newdata = test)
predictions

RMSE(predictions, test$Price)
MAE(predictions, test$Price)

library(Metrics)
mae(test$Price, predictions)
mdae(test$Price, predictions)
mape(test$Price, predictions)

#Task 5: Reducing the number of predictors
stepwise <- step(model, direction='both')

stepwise

stepwise$anova


model2 <- lm(Price ~ Age_08_04 + KM + Fuel_Type + HP + Doors + Quarterly_Tax + Weight
            , data = train)
summary(model2)

predictions2 <- predict(model2, newdata = test)
predictions2

RMSE(predictions2, test$Price)
MAE(predictions2, test$Price)
























## Mar 6 - Correspondence Analysis ####

library(ggplot2)
library(data.table)
library(MASS)
library(FactoMineR)

# Load the HairEyeColor in R.
data(HairEyeColor)
HE_dat = as.data.frame(HairEyeColor)
head(HE_dat)

# Use dcast to create two-way table. Ignore warning message.
HE.dt <- data.table::dcast(HE_dat, Hair ~ Eye, fun.aggregate = sum, value.var = "Freq")
rownames(HE.dt) <- HE.dt$Hair
HE.dt
HE.dt[,c(2:5)] # this is the subset of the 2-way table that will be used in the next step.

# Perform a chi-square test on the two-way table. Remember to select columns 2 to 5.
chisq <- chisq.test(HE.dt[,c(2:5)]) # the argument in this function always needs to be a contingency table
chisq


