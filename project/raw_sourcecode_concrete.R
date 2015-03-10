

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Import required libraries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
suppressMessages(library(ggplot2))       # General plotting functions
suppressMessages(library(doParallel))    # Parallel processing
suppressMessages(library(caret))         # Machine learning
suppressMessages(library(AppliedPredictiveModeling)) # Concrete data set
suppressMessages(library(randomForest))  # Machine learning

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
