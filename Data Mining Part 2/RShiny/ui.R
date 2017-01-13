#ui.R

ui <- fluidPage(
  titlePanel("Association Rules Mining"),
  sidebarPanel( 
    helpText("Show the association between each item in a 750000 transactions"),
    
    selectInput("Var",
                label = "Option :",
                choices = c("Graph","Scatterplot","Grouped Matrix"),
                selected = "Distribution of All Variables")
  ),
  mainPanel(plotOutput("transactions")))