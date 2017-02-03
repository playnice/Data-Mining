#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(ggplot2) 
library(ROCR)

load('classifiers.RO')
shinydata <- read.table("datatest2.txt",header=TRUE,sep=",",stringsAsFactors = FALSE)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$classifier <- renderPlot({
    Char <- input$choice
    if (Char == "Naive Bayers"){
      plot( NB, col = "blue", lwd = 2)
    }
    else if (Char == "Decision Tree"){
      plot ( DT , col = "yellow", lwd = 2)
    }
    else if (Char == "Artificial Neural Network"){
      plot ( ANN , col = "green", lwd = 2)
    }
    else if ( Char == 'All Classifiers'){
      plot( NB, col = "blue", lwd = 2)
      plot ( DT , add = "TRUE",col = "yellow", lwd = 2)
      plot ( ANN ,add = "TRUE", col = "green", lwd = 2)
      legend(0.6,0.6, legend=c("Naive Bayes","Decision Tree",  "Artificial Neural Network"),
             col=c("blue", "yellow", "green"), lty=1, cex=0.8, lwd = 2)
    }
    
    
    
  },height = 600,width = 600)
})
