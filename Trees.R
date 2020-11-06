install.packages("rpart")
install.packages("mlbench")
install.packages("dplyr")
install.packages("caret")
install.packages("party")
install.packages("randomForest")
install.packages("MASS")
install.packages("BART")
         
library(rpart)
library(mlbench)
library(dplyr)
library(caret)
library(party)
library(randomForest)
library(MASS)
library(BART)

 
# Load the data and remove NAs
data("PimaIndiansDiabetes2", package = "mlbench")
PimaIndiansDiabetes2 <- na.omit(PimaIndiansDiabetes2)
# Inspect the data
sample_n(PimaIndiansDiabetes2, 3)

# Split the data into training and test set
set.seed(123)
training.samples <- PimaIndiansDiabetes2$diabetes %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- PimaIndiansDiabetes2[training.samples, ]
test.data <- PimaIndiansDiabetes2[-training.samples, ]

 
# Build the model
set.seed(123)
model1 <- rpart(diabetes ~., data = train.data, method = "class")
# Plot the trees
par(xpd = NA) # Avoid clipping the text in some device
plot(model1)
text(model1, digits = 3)

# Make predictions on the test data
predicted.classes <- model1 %>% 
    predict(test.data, type = "class")
  head(predicted.classes)
  
# Compute model accuracy rate on test data
mean(predicted.classes == test.data$diabetes)
  
# Fit the model on the training set
set.seed(123)
model2 <- train(
    diabetes ~., data = train.data, method = "rpart",
    trControl = trainControl("cv", number = 10),
    tuneLength = 10
  )
# Plot model accuracy vs different values of
# cp (complexity parameter)
plot(model2)
  
# Print the best tuning parameter cp that
# maximizes the model accuracy
model2$bestTune
  
# Plot the final tree model
par(xpd = NA) # Avoid clipping the text in some device
plot(model2$finalModel)
text(model2$finalModel,  digits = 3)
  
# Decision rules in the model
model2$finalModel
  
# Make predictions on the test data
predicted.classes <- model2 %>% predict(test.data)
# Compute model accuracy rate on test data
mean(predicted.classes == test.data$diabetes)
  
# plot the pruned tree
plot(pfit, uniform=TRUE,
       main="Pruned Classification Tree for Kyphosis")
  text(pfit, use.n=TRUE, all=TRUE, cex=.8)

  
#conditional tree
model <- train(
    diabetes ~., data = train.data, method = "ctree2",
    trControl = trainControl("cv", number = 10),
    tuneGrid = expand.grid(maxdepth = 3, mincriterion = 0.95 )
  )
plot(model$finalModel)  
predicted.classes <- model %>% predict(test.data)

# Compute model accuracy rate on test data
mean(predicted.classes == test.data$diabetes)


#random forest
model <- train(
  diabetes ~., data = train.data, method = "rf",
  trControl = trainControl("cv", number = 10),
  importance = TRUE
)
# Best tuning parameter
model$bestTune

# Final model
model$finalModel
  
# Make predictions on the test data
predicted.classes <- model %>% predict(test.data)
head(predicted.classes)

# Compute model accuracy rate
mean(predicted.classes == test.data$diabetes)

importance(model$finalModel)

# Plot MeanDecreaseAccuracy
varImpPlot(model$finalModel, type = 1)
# Plot MeanDecreaseGini
varImpPlot(model$finalModel, type = 2)

varImp(model)



#Bayesian regression trees
head(Boston)
x = Boston[, c(6, 13)]
y = Boston$medv
head(cbind(x, y))
set.seed(99)
nd = 200
burn = 50
post = bart(x, y, nskip=burn, ndpost=nd)

names(post)
plot(post$sigma, type="l")
abline(v=burn, lwd=2, col="red")
lmf = lm(y~., data.frame(x, y))
fitmat = cbind(y, post$yhat.train.mean, lmf$fitted.values)
colnames(fitmat) = c("y", "BART", "Linear")
cor(fitmat)
pairs(fitmat)
i = order(post$yhat.train.mean)
boxplot(post$yhat.train[, i])



n = length(y)
set.seed(14)
i = sample(1:n, floor(0.75*n))
x.train = x[i, ]; y.train=y[i]
x.test = x[-i, ]; y.test=y[-i]
cat("training sample size = ", length(y.train), "\n")
cat("testing sample size = ", length(y.test), "\n")

set.seed(99)
post1 = wbart(x.train, y.train, x.test)
dim(post1$yhat.test)
length(post1$yhat.test.mean)

names(post1)
plot(post1$yhat.test.mean, type="l")
i = order(post1$yhat.test.mean)
boxplot(post1$yhat.test[, i])








