library(shiny)

# Define UI for random distribution application 
shinyUI(fluidPage(
        
        # Application title
        titlePanel("Predicting compressive strength of concrete mixtures"),
        
        # Sidebar with controls to select the random distribution type
        # and number of observations to generate. Note the use of the
        # br() element to introduce extra vertical spacing
        sidebarLayout(
                sidebarPanel(

                        
                        sliderInput("cement","Cement (Kg/m^3)", 17, 27, 20, 0.5),
                        sliderInput("age","Concrete Age (days)", 50, 80, 50, 0.5),
                        sliderInput("fine.aggregate","Fine Aggregate (Kg/m^3)", 50, 80, 50, 0.5),
                        sliderInput("coarse.aggregate","Coarse Aggregate (Kg/m^3)", 50, 80, 50, 0.5),
                        sliderInput("super.plasticizer","Super Plasticizer (Kg/m^3)", 50, 80, 50, 0.5),
                        sliderInput("water","Water (Kg/m^3)", 50, 80, 50, 0.5),
                        sliderInput("fly.ash","Fly Ash (Kg/m^3)", 50, 80, 50, 0.5),
                        sliderInput("blast.furnace.slag","Blast Furnace Slag (Kg/m^3)", 50, 80, 50, 0.5)
                ),
                
                # Show a tabset that includes a plot, summary, and table view
                # of the generated distribution
                mainPanel(
                        tabsetPanel(type = "tabs", 
                                    tabPanel("Prediction", verbatimTextOutput("text"), plotOutput("plot")),
                                    tabPanel("Residuals vs. Actual Strength", plotOutput("plot1")), 
                                    tabPanel("Residuals vs. Predicted Strength", plotOutput("plot2")), 
                                    tabPanel("Predicted Strength vs. Actual Strength", plotOutput("plot3")),
                                    tabPanel("Variable Importance", plotOutput("plot4")),
                                    tabPanel("About Application", plotOutput("plot"))
                        )
                )
        )
))