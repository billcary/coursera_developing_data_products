
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Import required libraries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(shiny)
suppressMessages(library(ggplot2))       # General plotting functions
suppressMessages(library(fmsb))          # Radar/spider plotting
suppressMessages(library(doParallel))    # Parallel processing
suppressMessages(library(caret))         # Machine learning
suppressMessages(library(AppliedPredictiveModeling)) # Concrete data set
suppressMessages(library(randomForest))  # Machine learning
suppressMessages(library(Metrics))       # Performance measures
diabetesRisk <- function(glucose) glucose / 200

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
## Train the model
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set.seed(300)

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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Provide initial values for the user input sliders
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sample <- data.frame(Cement = 293.0,
                     BlastFurnaceSlag = 6.5,
                     FlyAsh = 94.0,
                     Water = 200.0,
                     Superplasticizer = 16.1,
                     CoarseAggregate = 855.0,
                     FineAggregate = 800.0,
                     Age = 14.0)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ShinyServer function
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shinyServer(
        function(input, output) {
                
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                # Overall Model Performance plots
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                
                # Plot residuals vs. actual compressive strength
                output$plot1 <- renderPlot({
                        plot(concrete$CompressiveStrength, residuals.rf)
                })
                
                # Plot residuals vs. predicted strength
                output$plot2 <- renderPlot({
                        plot(predictions.rf, residuals.rf)
                })
                
                # Plot predictions vs. actual compressive strength
                output$plot3 <- renderPlot({
                        plot(concrete$CompressiveStrength, predictions.rf)
                })
                
                # Plot variable importance
                output$plot4 <- renderPlot({
                        varImpPlot(model.rf, type = 1, scale = TRUE)
                })
                
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                # Predict compressive strength using example parameters
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                sample <- data.frame(Cement = {input$cement},
                                     BlastFurnaceSlag = {input$blast.furnace.slag},
                                     FlyAsh = {input$fly.ash},
                                     Water = {input$water},
                                     Superplasticizer = {input$super.plasticizer},
                                     CoarseAggregate = {input$coarse.aggregate},
                                     FineAggregate = {input$fine.aggregate},
                                     Age = {input$age})
                
                predict.sample <- predict(model.rf, sample, predict.all = TRUE)
                
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                # Render outputs to screen
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                # Render output prediction and confidence interval
                
                # Render user input values
                
                # Render radar plot of user input values
                
                # Plot variable importance
                output$plot4 <- renderPlot({
                        varImpPlot(model.rf, type = 1, scale = TRUE)
                })
                output$inputValue <- renderPrint({input$glucose})
                output$prediction <- renderPrint({diabetesRisk(input$glucose)})
        }
)
