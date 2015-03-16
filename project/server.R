library(shiny)
suppressMessages(library(fmsb))          # Radar/spider plotting
suppressMessages(library(caret))         # Machine learning
suppressMessages(library(AppliedPredictiveModeling)) # Concrete data set
suppressMessages(library(randomForest))  # Machine learning



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ShinyServer function
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Define server logic for random distribution application
shinyServer(function(input, output) {
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Read the data into R
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        data("concrete")
        
        data <- concrete
        
#         ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#         ## Determine min/max for each column
#         ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#         output$cement.min <- renderText({min(concrete$Cement)})
#         output$blast.furnace.slag.min <- renderText({min(concrete$BlastFurnaceSlag)})
#         output$fly.ash.min <- renderText({min(concrete$FlyAsh)})
#         output$water.min <- renderText({min(concrete$Water)})
#         output$super.plasticizer.min <- renderText({min(concrete$Superplasticizer)})
#         output$course.aggregate.min <- renderText({min(concrete$CoarseAggregate)})
#         output$fine.aggregate.min <- renderText({min(concrete$FineAggregate)})
#         output$age.min <- renderText({min(concrete$Age)})
#         
#         output$cement.max <- renderText({max(concrete$Cement)})
#         output$blast.furnace.slag.max <- renderText({max(concrete$BlastFurnaceSlag)})
#         output$fly.ash.max <- renderText({max(concrete$FlyAsh)})
#         output$water.max <- renderText({max(concrete$Water)})
#         output$super.plasticizer.max <- renderText({max(concrete$Superplasticizer)})
#         output$course.aggregate.max <- renderText({max(concrete$CoarseAggregate)})
#         output$fine.aggregate.max <- renderText({max(concrete$FineAggregate)})
#         output$age.max <- renderText({max(concrete$Age)})
        
        
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
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Provide initial values for the user input sliders
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        sample.data <- data.frame(Cement = 200.0,
                                  BlastFurnaceSlag = 150.0,
                                  FlyAsh = 100.0,
                                  Water = 200.0,
                                  Superplasticizer = 10.0,
                                  CoarseAggregate = 900.0,
                                  FineAggregate = 700.0,
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
                plot(predictions.rf$aggregate, residuals.rf)
        })
        
        # Plot predictions vs. actual compressive strength
        output$plot3 <- renderPlot({
                plot(concrete$CompressiveStrength, predictions.rf$aggregate)
        })
        
        # Plot variable importance
        output$plot4 <- renderPlot({
                varImpPlot(model.rf, type = 1, scale = TRUE)
        })
        
        output$text1 <- renderText({sample.data$Cement})
        
        output$text2 <- renderText({'text2'})
        
        
        # Reactive expression to generate the requested distribution.
        # This is called whenever the inputs change. The output
        # functions defined below then all use the value computed from
        # this expression
        get.input.data <- reactive({
                
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                # Predict compressive strength using example parameters
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                sample.data <- data.frame(Cement = {input$cement},
                                          BlastFurnaceSlag = {input$blast.furnace.slag},
                                          FlyAsh = {input$fly.ash},
                                          Water = {input$water},
                                          Superplasticizer = {input$super.plasticizer},
                                          CoarseAggregate = {input$coarse.aggregate},
                                          FineAggregate = {input$fine.aggregate},
                                          Age = {input$age})
        })
        
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Set up a dataframe containing the maximum and minimum
        # column values as rows 1 and 2, and the values the users
        # inputs for prediction as row 3.  This format is 
        # required by radarchart (below) to format the plot properly.
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        plot.data <- rbind(c(450.0, 350.0, 200.0, 245.0, 30.0, 1100.0, 950.0, 365.0),
                           c(150.0, 150.0, 100.0, 125.0, 0.0, 800.0, 600.0, 1.0),
                           sample.data)
        
        # Render radar plot of user input values
        output$plot5 <- renderPlot({
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                # Plot the user-input values on a radar chart.
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                radarchart(plot.data,
                           maxmin = TRUE,
                           centerzero = TRUE,
                           pfcol = c(5),
                           pcol = c(4),
                           pty = c(16),
                           plty = c(7),
                           cglcol = 'red',
                           title = 'Current Concrete Composition Selection')
        })
        
        predict.sample <- predict(model.rf, sample.data, predict.all = TRUE)
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Calculate the 95% confidence interval for the prediction based
        # on the user's input values.
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        predict.sample.int <- apply( predict.sample$individual, 1, function(x) {
                c( quantile(x, c(0.025)),
                   mean(x),
                   quantile(x, c(0.975)) )
        })
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Render outputs to screen
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Render output prediction and confidence interval
        output$prediction.text <- renderText({paste("Based on the chosen parameters,
                                                            the predicted compressive strength
                                                            is ", round(predict.sample.int[2], 2), ".")
        })
        
        output$prediction.ci <- renderText({paste("The confidence interval
                                                          for the prediction is ",
                                                  round(predict.sample.int[1], 2),
                                                  " to ", 
                                                  round(predict.sample.int[3], 2),
                                                  ".")
        })
        
        # Render user input values
        output$input.vals <- renderText({paste("The selected input 
                                                       values are as follows: ",
                                               sample)
        })
        
})