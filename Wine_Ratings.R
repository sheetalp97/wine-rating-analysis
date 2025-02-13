##################################################
#   ISDS 540 - GROUP 02
#   FINAL PROJECT SCRIPT
#   Rating Wines from the Piedmont Region of Italy
##################################################



#### Importing necessary libraries ####

library(ggplot2)



#### Look at the data ####

rm(list=ls())
Wine <- read.csv("D:/Sheetal/CSUF/Fall'23/ISDS 540/Group Project/WineRatings.csv")
head(Wine, 10)
WineNew <- Wine[,(3:4)]
head(WineNew, 10)



#### Plots for Score ~ Price ####

## Histogram

hist(WineNew$Score, xlim=c(76, 100), ylim=c(0, 30), xlab="Score",
     main="Figure 1: Histogram for Score of Wines")

## Boxplot

boxplot(WineNew$Score, xlab = 'Score', ylab = 'Frequency')

boxplot(WineNew$Price, xlab = 'Price', ylab = 'Frequency') 
# Price variable is skewed # 

boxplot(Score ~ Price, data = WineNew, col = c("red", "blue")) 

## Scatterplot

ggplot(WineNew, aes(Price, Score)) + labs(title = "Scatter plot with regression line of Score vs Price") + geom_point() + geom_smooth()
# Price and Score have kind of a Quadratic relation #



#### Fit Initial Model ####

Wine.fit <- lm(Score ~ Price, data = WineNew)
summary(Wine.fit)



#### Creating new variables ####

WineNew$Price_Log = log(WineNew$Price)
WineNew$Score_Log = log(WineNew$Score)
WineNew$Price_Sq = (WineNew$Price)^2
WineNew$Score_Sq = (WineNew$Score)^2



#### Fixing the train and test data sets ####

n = nrow(WineNew)
p = ncol(WineNew)
set.seed(456)

train.index <- sample(row.names(WineNew), floor(0.8*n))  
test.index <- setdiff(row.names(WineNew), train.index)  
train.df <- WineNew[train.index,]  
test.df <- WineNew[test.index,]



##################################################
#   MODEL 1
##################################################


#### Training and fitting Model 1 for train and test data sets ####

# Training the model

mod1 <- lm(Score ~ Price, data = train.df)
summary(mod1)

# Fit the model on test set

mod1_test <- lm(Score ~ Price, data = test.df)
summary(mod1_test)

# Model 1 RMSE

preds.mod1 <- predict(mod1, newdata = test.df)
MSE1 <- mean((preds.mod1 - test.df$Score)^2) 

RMSE1 <- sqrt(MSE1)
print(RMSE1)



##################################################
#   MODEL 2
##################################################


#### Training and fitting Model 2 for train and test data sets ####

# Training the model

mod2 <- lm(Score_Sq ~ Price_Log, data = train.df)
summary(mod2)

# Fit the model on test set

mod2_test <- lm(Score_Sq ~ Price_Log, data = test.df)
summary(mod2_test)

# Model 2 RMSE

preds.mod2 <- predict(mod2, newdata = test.df)
preds2.Score <- sqrt(preds.mod2)
MSE2 <- mean((preds2.Score - test.df$Score)^2) 

RMSE2 <- sqrt(MSE2)
print(RMSE2)



##################################################
#   MODEL 3
##################################################


#### Training and fitting Model 3 for train and test data sets ####

# Training the model

mod3 <- lm(Score ~ Price_Log + Price_Sq, data = train.df)
summary(mod3)

# Fit the model on test set

mod3_test <- lm(Score ~ Price_Log + Price_Sq, data = test.df)
summary(mod3_test)

# Model 3 RMSE

preds.mod3 <- predict(mod3, newdata = test.df)
MSE3 <- mean((preds.mod3 - test.df$Score)^2) 

RMSE3 <- sqrt(MSE3)
print(RMSE3)



##################################################
#   MODEL 4
##################################################


#### Training and fitting Model 4 for train and test data sets ####

# Training the model

mod4 <- lm(Score ~ Price_Log, data = train.df)
summary(mod4)

# Fit the model on test set

mod4_test <- lm(Score ~ Price_Log, data = test.df)
summary(mod4_test)

# Model 4 RMSE

preds.mod4 <- predict(mod4, newdata = test.df)
MSE4 <- mean((preds.mod4 - test.df$Score)^2) 

RMSE4 <- sqrt(MSE4)
print(RMSE4)

# Scatter Plot for Score ~ Log(Price)

ggplot(WineNew, aes(Price_Log, Score)) + 
  labs(title = "Scatter plot with regression line of Score vs Log(Price)", x = "Log(Price)", y = "Score") + 
  geom_point() + geom_smooth()



##################################################
#   MODEL 5
##################################################


#### Training and fitting Model 5 for train and test data sets ####

# Training the model

mod5 <- lm(Score_Log ~ Price_Log, data = train.df)
summary(mod5)

# Fit the model on test set

mod5_test <- lm(Score_Log ~ Price_Log, data = test.df)
summary(mod5_test)

# Model 5 RMSE

preds.mod5 <- predict(mod5, newdata = test.df)
preds5.Score = exp(preds.mod5)
MSE5 <- mean((preds5.Score - test.df$Score)^2) 

RMSE5 <- sqrt(MSE5)
print(RMSE5)

# Scatter Plot for Log(Score) ~ Log(Price)

ggplot(WineNew, aes(Price_Log, Score_Log)) + 
  labs(title = "Scatter plot with regression line of Log(Score) vs Log(Price)", x = "Log(Price)", y = "Log(Score)") + 
  geom_point() + geom_smooth()



#### Printing Mean Squared Error and Root Mean Squared Error for all models ####

print(c(MSE1, MSE2, MSE3, MSE4, MSE5))
print(c(RMSE1, RMSE2, RMSE3, RMSE4, RMSE5))



##################################################
#   FINAL MODEL
##################################################


#### Final model will be used to predict Score ####

full.df = rbind(train.df, test.df)
mod.final <- lm(Score_Log ~ Price_Log, data = full.df)
summary(mod.final)