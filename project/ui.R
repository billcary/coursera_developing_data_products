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
                                    150,
                                    450,
                                    200,
                                    25),
                        sliderInput("age","Concrete Age (days)",
                                    1,
                                    365,
                                    28,
                                    1),
                        sliderInput("fine.aggregate","Fine Aggregate (Kg/m^3)", 
                                    600,
                                    950,
                                    700,
                                    10),
                        sliderInput("coarse.aggregate","Coarse Aggregate (Kg/m^3)",
                                    800, 
                                    1100, 
                                    900, 
                                    10),
                        sliderInput("super.plasticizer","Super Plasticizer (Kg/m^3)",
                                    0,
                                    30,
                                    10,
                                    5),
                        sliderInput("water","Water (Kg/m^3)",
                                    125,
                                    245,
                                    200,
                                    10),
                        sliderInput("fly.ash","Fly Ash (Kg/m^3)",
                                    0,
                                    200,
                                    100,
                                    10),
                        sliderInput("blast.furnace.slag","Blast Furnace Slag (Kg/m^3)",
                                    0,
                                    350,
                                    150,
                                    25)
                ),
                
                # Show a tabset that includes a plot, summary, and table view
                # of the generated distribution
                mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("About Application",
                                             pre(includeText("about.txt")
                                             )),
                                    tabPanel("Prediction",
                                             textOutput("prediction.text"),
                                             br(),
                                             textOutput("prediction.ci"),
                                             br(),
                                             column(4,tableOutput("input.vals")),
                                             column(4, plotOutput("plot5"))
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
                                    )
                        )
                )
        )
))