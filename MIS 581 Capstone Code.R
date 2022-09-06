install.packages("ggplot2")
install.packages("plotly")
install.packages("rpart")
install.packages("rpart.plot")
library(ggplot2)
library(plotly)
library(rpart)
library(rpart.plot)

#upload data to R studio. Create data summary and see how many missing files there are

cc.raw <-Cervical_Cancer
str(cc.raw)

missingfile <- is.na(cc.raw)
sum(missingfile)

summary(cc.raw)

# pull out important variables for analysis
# remove missing data

cc.trim <- Cervical_Cancer[, c(1,2,3,4,5,6,7,8,9,10,11,12,13,18,19,20,21,22,23,24,25,26,29,30,31,32,33,34,35,36)]
cc.trimnna <- na.omit(cc.trim)

cc.trim2 <- Cervical_Cancer[, c(1,2,3,4,5,6,7,8,9,10,11,12,13,18,19,20,23,24,25,26,29,30,31,32,33,34,35,36)]
cc.trimnna2 <- na.omit(cc.trim2)

# create a correlation matrix for all of the data based on summary results for other data some variables are omitted

correlationccc <- cor(cc.trimnna2)
print(correlationccc)

# logistic regression model

logit.cc <- glm(cc.trimnna2$Dx ~., data = cc.trimnna)
options(scipen = 999)
summary(logit.cc)

# individual logistic regression model

logit.cancer.cc <- glm(cc.trimnna2$Dx ~ cc.trimnna2$Dx_Cancer)
logit.cin.cc <- glm(cc.trimnna2$Dx ~ cc.trimnna2$Dx_CIN)
logit.hpv.cc <- glm(cc.trimnna2$Dx ~ cc.trimnna2$Dx_HPV)
summary(logit.cancer.cc)
summary(logit.cin.cc)
summary(logit.hpv.cc)

# pull out the most significant variables to create a logistic regression model

cc.sig <- Cervical_Cancer[, c(7,9,10,29,30,31,32)]
cc.sig.clean <- na.omit(cc.sig)

logit.cc.sig <- glm(cc.sig.clean$Dx ~., data = cc.sig.clean)
summary(logit.cc.sig)

cc.data <- cc.trim2

# partition the data 

set.seed(1)
train.cc.rows <- sample(rownames(cc.data), dim(cc.data)[1]*0.6)
valid.cc.rows <- sample(setdiff(rownames(cc.data), train.cc.rows),dim(cc.data)[1]*0.4)

train.cc.data <- cc.data[train.cc.rows, ]
valid.cc.data <- cc.data[valid.cc.rows, ]

#ensure that the randomization is correct

prop.table(table(train.cc.data$Dx))
prop.table(table(valid.cc.data$Dx))

# create a decision tree model for train and validation data

fit.train <- rpart(train.cc.data$Dx ~., data = train.cc.data, method = 'class', control = rpart.control(cp = 0.001, maxdepth = 10, minsplit = 2))
rpart.plot(fit.train)
fit.train

fit.valid <- rpart(valid.cc.data$Dx ~., data = valid.cc.data, method = 'class', control = rpart.control(cp = 0.001, maxdepth = 10, minsplit = 2))
rpart.plot(fit.valid)
fit.valid

fit <- rpart(cc.data$Dx ~., data = cc.data, method = 'class', control = rpart.control(cp = 0.001, maxdepth = 10, minsplit = 2))
rpart.plot(fit)
fit

# find the accuracy of the model

predict_train <- predict(fit.train, train.cc.data, type = 'class')

table_train <- table(train.cc.data$Dx, predict_train)
table_train

accuracy_train <- sum(diag(table_train)) / sum(table_train)

print(paste('Accuracy for the test', accuracy_train))

predict_valid <- predict(fit.valid, valid.cc.data, type = 'class')

table_valid <- table(valid.cc.data$Dx, predict_valid)
table_valid

accuracy_valid <- sum(diag(table_valid)) / sum(table_valid)

print(paste('Accuracy for the test', accuracy_valid))
