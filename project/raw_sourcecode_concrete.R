

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Import required libraries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
suppressMessages(library(ggplot2))       # General plotting functions
suppressMessages(library(fmsb))          # Radar/spider plotting
suppressMessages(library(doParallel))    # Parallel processing
suppressMessages(library(caret))         # Machine learning
suppressMessages(library(AppliedPredictiveModeling)) # Concrete data set
suppressMessages(library(randomForest))  # Machine learning
suppressMessages(library(Metrics))       # Performance measures

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Register parallel backend
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cl <- makeCluster(4)  # set appropriately for server on which job will run
registerDoParallel(cl)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read the data into R
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data <- data("concrete")

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Holdout 20% of data for testing estimates
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set.seed(107)
inTrain <- createDataPartition(y = concrete$CompressiveStrength,
                               p = 0.80,
                               list = FALSE)

training <- concrete[inTrain,]
testing <- concrete[-inTrain,]

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Establish training control and tuning parameters
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Set up training control parameters (10-fold repeated cross validation)
fitControl <- trainControl(method = "repeatedcv",
                           number = 10, ## 10-fold CV
                           repeats = 3) ## repeated three times

grid <- expand.grid(mtry = c(2, 4, 6, 8))

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Train model
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set.seed(300)

model <- train(CompressiveStrength ~ .
               ,data = concrete
               ,model = 'rf'
               ,trControl = fitControl
               ,tuneGrid = grid
               ,importance = TRUE
               ,ntree = 1000)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Print the Model and Model Summary  
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print(model)
summary(model)
varImp(model)
plot(varImp(model))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Performance plots
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
residuals <- resid(model)
predictions <- predict(model)
plot(concrete$CompressiveStrength, residuals)
plot(predictions, residuals)
plot(concrete$CompressiveStrength, predictions)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Retrain model using randomForest package w/o caret (for use on shiny.io)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
model.rf <- randomForest(CompressiveStrength ~ .
               ,data = training
               ,xtest = testing[, 1:8]
               ,ytest = testing$CompressiveStrength
               ,mtry = 6
               ,ntree = 750
               ,importance = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Performance plots
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
predictions.rf <- predict(model.rf, concrete, predict.all = TRUE)
residuals.rf <- concrete$CompressiveStrength - predictions.rf
plot(concrete$CompressiveStrength, residuals.rf)
plot(predictions.rf, residuals.rf)
plot(concrete$CompressiveStrength, predictions.rf)
varImpPlot(model.rf, type = 1, scale = TRUE)

pred.rf.int <- apply( predictions.rf$individual, 1, function(x) {
        c( quantile(x, c(0.025)),
           mean(x),
           quantile(x, c(0.975)) )
        })

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Predict compressive strength using example parameters
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sample <- concrete[85,]
sample$Cement <- 293.0
sample$BlastFurnaceSlag <- 6.5
sample$FlyAsh <- 94.00
sample$Water <- 200.0
sample$Superplasticizer <- 16.100
sample$CoarseAggregate <- 855.0
sample$FineAggregate <- 800.0
sample$Age <- 14.00
sample$CompressiveStrength <- NULL

predict.sample <- predict(model.rf, sample, predict.all = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculate the 95% confidence interval for the prediction based on the user's
# input values.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
predict.sample.int <- apply( predict.sample$individual, 1, function(x) {
        c( quantile(x, c(0.025)),
           mean(x),
           quantile(x, c(0.975)) )
})

predict.sample.int

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set up a dataframe containing the maximum and minimum column values as rows
# 1 and 2, and the values the users inputs for prediction as row 3.  This format
# is required by radarchart (below) to format the plot properly.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot.data <- rbind(colMaxs(as.matrix(concrete)),
                   colMins(as.matrix(concrete)),
                   sample)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot the user-input values on a radar chart.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
radarchart(plot.data,
           maxmin = TRUE,
           centerzero = TRUE,
           pfcol = c(5),
           pcol = c(4),
           pty = c(16),
           plty = c(7),
           cglcol = 'red',
           title = 'Current Concrete Composition Selection')
