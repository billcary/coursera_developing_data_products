library(shiny)
suppressMessages(library(fmsb))          # Radar/spider plotting
suppressMessages(library(caret))         # Machine learning
suppressMessages(library(AppliedPredictiveModeling)) # Concrete data set
suppressMessages(library(randomForest))  # Machine learning

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read the data into R
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data("concrete")

data <- concrete

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Holdout 20% of data for testing estimates
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set.seed(107)
inTrain <- createDataPartition(y = concrete$CompressiveStrength,
                               p = 0.80,
                               list = FALSE)

training <- concrete[inTrain,]
testing <- concrete[-inTrain,]

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Train the model
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set.seed(300)

model.rf <- randomForest(CompressiveStrength ~ .
                         ,data = training
                         ,xtest = testing[, 1:8]
                         ,ytest = testing$CompressiveStrength
                         ,mtry = 6
                         ,ntree = 750
                         ,importance = TRUE
                         ,keep.forest = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Performance plots
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
predictions.rf <- predict(model.rf, concrete, predict.all = TRUE)

residuals.rf <- concrete$CompressiveStrength - predictions.rf$aggregate

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ShinyServer function
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Define server logic for random distribution application
shinyServer(function(input, output) {
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Provide initial values for the user input sliders
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        sample <- data.frame(Cement = 293.0,
                             BlastFurnaceSlag = 6.5,
                             FlyAsh = 94.0,
                             Water = 200.0,
                             Superplasticizer = 16.1,
                             CoarseAggregate = 855.0,
                             FineAggregate = 800.0,
                             Age = 14.0)
        
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
        # Calculate the 95% confidence interval for the prediction based
        # on the user's input values.
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        predict.sample.int <- apply( predict.sample$individual, 1, function(x) {
                c( quantile(x, c(0.025)),
                   mean(x),
                   quantile(x, c(0.975)) )
        })
        
        # Reactive expression to generate the requested distribution.
        # This is called whenever the inputs change. The output
        # functions defined below then all use the value computed from
        # this expression
        data <- reactive({
                dist <- switch(input$dist,
                               norm = rnorm,
                               unif = runif,
                               lnorm = rlnorm,
                               exp = rexp,
                               rnorm)
                
                dist(input$n)
        })
        
        # Generate a plot of the data. Also uses the inputs to build
        # the plot label. Note that the dependencies on both the inputs
        # and the data reactive expression are both tracked, and
        # all expressions are called in the sequence implied by the
        # dependency graph
        output$plot <- renderPlot({
                dist <- input$dist
                n <- input$n
                
                hist(data(), 
                     main=paste('r', dist, '(', n, ')', sep=''))
        })
        
        # Generate a summary of the data
        output$summary <- renderPrint({
                summary(data())
        })
        
        # Generate an HTML table view of the data
        output$table <- renderTable({
                data.frame(x=data())
        })
        
})