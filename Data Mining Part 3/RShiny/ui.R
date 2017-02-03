#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
ui <- fluidPage(
  
  # Application title
  titlePanel("Data Mining Classifier"),
  
  # Sidebar with a slider input for number of bins 
  
  sidebarPanel(
    helpText("Show the performance of each classifiers."),
    
    selectInput("choice",
                label = "Option :",
                choices = c("Naive Bayers","Decision Tree","Artificial Neural Network", "All Classifiers"),
                selected = "Naive Bayers")
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("classifier")))
