library(shiny)

# Define UI for dataset viewer application
shinyUI(navbarPage("Predicting compressive strength of concrete mixtures",
                   inverse = TRUE, collapsable = FALSE, fluid = TRUE, responsive = TRUE,
                   tabPanel("About App",
                            fluidRow(includeHTML("AboutApp.html"))),
                   tabPanel("Predict", # Sidebar with a slider input for number of bins
                            sidebarLayout(
                                    sidebarPanel("Use sliders to choose the amount of each concrete component",
                                                 sliderInput("cement","Cement (Kg/m^3)", 17, 27, 20, 0.5),
                                                 sliderInput("age","Concrete Age (days)", 50, 80, 50, 0.5),
                                                 sliderInput("fine.aggregate","Fine Aggregate (Kg/m^3)", 50, 80, 50, 0.5),
                                                 sliderInput("coarse.aggregate","Coarse Aggregate (Kg/m^3)", 50, 80, 50, 0.5),
                                                 sliderInput("super.plasticizer","Super Plasticizer (Kg/m^3)", 50, 80, 50, 0.5),
                                                 sliderInput("water","Water (Kg/m^3)", 50, 80, 50, 0.5),
                                                 sliderInput("fly.ash","Fly Ash (Kg/m^3)", 50, 80, 50, 0.5),
                                                 sliderInput("blast.furnace.slag","Blast Furnace Slag (Kg/m^3)", 50, 80, 50, 0.5),),
                                    mainPanel(            
                                            verbatimTextOutput("predict"),
                                            textOutput("text1"),
                                            plotOutput("plot1")))),
                   navbarMenu("Model Performance",
                              tabPanel("Actual Strength vs. Residuals",   
                                       verbatimTextOutput("summary")),
                              tabPanel("Predicted Strength vs. Residuals",   
                                       plotOutput("plot1")),
                              tabPanel("Actual Strength vs. Predicted Strength",   
                                       plotOutput("plot2")),
                              tabPanel("Variable Importance",   
                                       plotOutput("plot3"))),
                   tabPanel("About Stackloss", 
                            fluidRow(includeHTML("AboutStackloss.html")))        
                   
))

