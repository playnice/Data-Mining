#ui.R

ui <- fluidPage(
  titlePanel("Wine Quality"),
  sidebarPanel( 
    helpText("Show the information of chemical properties of wine."),
    
    selectInput("Var",
                label = "Choose a variable to display :",
                choices = c("Distribution of All Variables","Correlation","Outliers"),
                selected = "Distribution of All Variables")
  ),
  mainPanel(plotOutput("barChart")))