#server.R

#Please run these four line before run the web App
library(shiny)
library(ggplot2)
library(corrplot)
wine <- read.csv("winequality-white.csv", header = TRUE, sep=';',check.names = FALSE)
colnames<-dimnames(wine)[[2]]
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

server <- function(input,output){

  output$barChart<-renderPlot({
    
    Char <- input$Var
    
    if(Char == "Distribution of All Variables")
    {
      p1 <- ggplot(wine, aes(x= wine[,1])) +
            geom_bar(position = "dodge",width = 0.5,colour = 'blue')+
            xlab("Fixed Acidity")+
            ylab("Total Count")
     
       p2 <- ggplot(wine, aes(x= wine[,2])) +
             geom_bar(position = "dodge",width = 0.1, colour='blue')+
             xlab("Volatile Acidity")+
             ylab("Total Count")
      
       p3 <- ggplot(wine, aes(x= wine[,3])) +
             geom_bar(position = "dodge",width = 0.05,colour='blue')+
             xlab("Citric Acid")+
             ylab("Total Count")
       
       p4 <- ggplot(wine, aes(x= wine[,4])) +
             geom_bar(position = "dodge",width = 1,colour='blue')+
             xlab("Residal Sugar")+
             ylab("Total Count")
       p5 <- ggplot(wine, aes(x= wine[,5])) +
             geom_bar(position = "dodge",width = 0.005,colour='blue')+
             xlab("Chlorides")+
             ylab("Total Count")       
       p6 <- ggplot(wine, aes(x= wine[,6])) +
             geom_bar(position = "dodge",width = 2,colour='blue')+
             xlab("Free Sulphur Dioxide")+
             ylab("Total Count")    
       p7 <- ggplot(wine, aes(x= wine[,7])) +
             geom_bar(position = "dodge",width = 10,colour='blue')+
             xlab("Total Sulphur Dioxide")+
             ylab("Total Count")   
       p8 <- ggplot(wine, aes(x= wine[,8])) +
             geom_bar(position = "dodge",width = 0.01,colour='blue')+
             xlab("Density")+
             ylab("Total Count")  
       p9 <- ggplot(wine, aes(x= wine[,9])) +
             geom_bar(position = "dodge",width = 0.2,colour='blue')+
             xlab("pH")+
             ylab("Total Count")   
       
       p10 <- ggplot(wine, aes(x= wine[,10])) +
              geom_bar(position = "dodge",width = 0.2,colour='blue')+
              xlab("Sulphates")+
              ylab("Total Count") 
       p11 <- ggplot(wine, aes(x= wine[,11])) +
              geom_bar(position = "dodge",width = 1,colour='blue')+
              xlab("Alcohol")+
              ylab("Total Count")  
       p12 <- ggplot(wine, aes(x= wine[,12])) +
              geom_bar(position = "dodge",width = 1,colour='blue')+
              xlab("Quality")+
              ylab("Total Count")   
       
      multiplot(p1, p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,cols=4)
      
    }
    else if (Char == "Correlation")
    {
      corrplot(cor(wine[1:12]), method="number")
    }
    else if (Char == "Outliers")
    {
      p1 <- ggplot(wine, aes( factor(0), wine[,1])) + 
            geom_boxplot(fill = "white", colour = "#3366FF")+ 
            ylab(NULL)+
            xlab(NULL)+
            ggtitle("Fixed Acidity")+
            theme(plot.title = element_text(lineheight=.8, face="bold"))
      p2 <- ggplot(wine, aes( factor(0), wine[,2])) + 
            geom_boxplot(fill = "white", colour = "#3366FF")+ 
            ylab(NULL)+
            xlab(NULL)+
            ggtitle("Volatile Acidity")+
            theme(plot.title = element_text(lineheight=.8, face="bold"))
      p3 <- ggplot(wine, aes( factor(0), wine[,3])) + 
            geom_boxplot(fill = "white", colour = "#3366FF")+ 
            ylab(NULL)+
            xlab(NULL)+
            ggtitle("Citric Acid")+
            theme(plot.title = element_text(lineheight=.8, face="bold"))
      p4 <- ggplot(wine, aes( factor(0), wine[,4])) + 
            geom_boxplot(fill = "white", colour = "#3366FF")+ 
            ylab(NULL)+
            xlab(NULL)+
            ggtitle("Residal Sugar")+
            theme(plot.title = element_text(lineheight=.8, face="bold"))
      p5 <- ggplot(wine, aes( factor(0), wine[,5])) + 
            geom_boxplot(fill = "white", colour = "#3366FF")+ 
            ylab(NULL)+
            xlab(NULL)+
            ggtitle("Chlorides")+
            theme(plot.title = element_text(lineheight=.8, face="bold"))
      p6 <- ggplot(wine, aes( factor(0), wine[,6])) + 
            geom_boxplot(fill = "white", colour = "#3366FF")+ 
            ylab(NULL)+
            xlab(NULL)+
            ggtitle("Free Sulphur Dioxide")+
            theme(plot.title = element_text(lineheight=.8, face="bold"))
      p7 <- ggplot(wine, aes( factor(0), wine[,7])) + 
            geom_boxplot(fill = "white", colour = "#3366FF")+ 
            ylab(NULL)+
            xlab(NULL)+
            ggtitle("Total Sulphur Dioxide")+
            theme(plot.title = element_text(lineheight=.8, face="bold"))
      p8 <- ggplot(wine, aes( factor(0), wine[,8])) + 
            geom_boxplot(fill = "white", colour = "#3366FF")+ 
            ylab(NULL)+
            xlab(NULL)+
            ggtitle("Density")+
            theme(plot.title = element_text(lineheight=.8, face="bold"))
      p9 <- ggplot(wine, aes( factor(0), wine[,9])) + 
            geom_boxplot(fill = "white", colour = "#3366FF")+ 
            ylab(NULL)+
            xlab(NULL)+
            ggtitle("pH")+
            theme(plot.title = element_text(lineheight=.8, face="bold"))
      p10 <- ggplot(wine, aes( factor(0), wine[,10])) + 
             geom_boxplot(fill = "white", colour = "#3366FF")+ 
             ylab(NULL)+
             xlab(NULL)+
             ggtitle("Sulphates")+
             theme(plot.title = element_text(lineheight=.8, face="bold"))
      p11 <- ggplot(wine, aes( factor(0), wine[,11])) + 
             geom_boxplot(fill = "white", colour = "#3366FF")+ 
             ylab(NULL)+
             xlab(NULL)+
             ggtitle("Alcohol")+
             theme(plot.title = element_text(lineheight=.8, face="bold"))
      multiplot(p1, p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,cols=4)
    }
    
    },height = 600,width = 600)}