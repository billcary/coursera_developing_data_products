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
                        
                        sliderInput("cement","Cement (Kg/m^3)",
                                    cement.min,
                                    cement.max,
                                    mean(cement.min, cement.max),
                                    0.5),
                        sliderInput("age","Concrete Age (days)",
                                    age.min,
                                    age.max,
                                    mean(age.min, age.max),
                                    0.5),
                        sliderInput("fine.aggregate","Fine Aggregate (Kg/m^3)", 
                                    fine.aggregate.min,
                                    fine.aggregate.max,
                                    mean(fine.aggregate.min, fine.aggregate.max),
                                    0.5),
                        sliderInput("coarse.aggregate","Coarse Aggregate (Kg/m^3)",
                                    course.aggregate.min, 
                                    course.aggregate.max, 
                                    mean(course.aggregate.min, course.aggregate.max), 
                                    0.5),
                        sliderInput("super.plasticizer","Super Plasticizer (Kg/m^3)",
                                    super.plasticizer.min,
                                    super.plasticizer.max,
                                    mean(super.plasticizer.min, super.plasticizer.max),
                                    0.5),
                        sliderInput("water","Water (Kg/m^3)",
                                    water.min,
                                    water.max,
                                    mean(water.min, water.max),
                                    0.5),
                        sliderInput("fly.ash","Fly Ash (Kg/m^3)",
                                    fly.ash.min,
                                    fly.ash.max,
                                    mean(fly.ash.min, fly.ash.max),
                                    0.5),
                        sliderInput("blast.furnace.slag","Blast Furnace Slag (Kg/m^3)",
                                    blast.furnace.slag.min,
                                    blast.furnace.slag.max,
                                    mean(blast.furnace.slag.min, blast.furnace.slag.max),
                                    0.5)
                ),
                
                # Show a tabset that includes a plot, summary, and table view
                # of the generated distribution
                mainPanel(
                        tabsetPanel(type = "tabs", 
                                    tabPanel("Prediction",
                                             textOutput("text1"),
                                             plotOutput("plot5")
                                             ),
                                    
                                    tabPanel("Residuals vs. Actual Strength",
                                             plotOutput("plot1")
                                             ), 
                                    
                                    tabPanel("Residuals vs. Predicted Strength",
                                             plotOutput("plot2")
                                             ), 
                                    
                                    tabPanel("Predicted Strength vs. Actual Strength",
                                             plotOutput("plot3")
                                             ),
                                    
                                    tabPanel("Variable Importance",
                                             plotOutput("plot4")
                                             ),
                                    
                                    tabPanel("About Application",
                                             textOutput("text2")
                                             )
                        )
                )
        )
))