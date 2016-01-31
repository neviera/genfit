#function to time series variable
#this function generates time series variable with various components
#function takes input n - number of data points, returns dataframe

generateData <- function(n=100){
  #number 
  var.names<-c('Instance','Variable')
  
  #seed is always dynamic
  set.seed( as.integer((as.double(Sys.time())*1000+Sys.getpid()) %% 2^31) )
  
  #trend component
  seqa<-data.frame(seq(1,n,1)/n,seq(n,1,-1)/n)
  wh <- sample(c(1,2),1)
  trend <- seqa[,wh]
  trend <- trend*runif(1,-5,5)
  
  #intercetp
  inter <- runif(1,-2,2)
  
  #error
  error<-rnorm(n,2,0.5)
  
  #linear
  linear <- inter + runif(1,-1,1)*trend + error
  
  #qudratic compenent
  quad <- inter + runif(1,-1,1)*trend + runif(1,-1,1)*trend^2 + error^2
  
  #cubic component
  cubic<- inter + runif(1,-1,1)*trend + runif(1,-1,1)*trend^2 + runif(1,-1,1)*trend^3 + error^3
  
  #exponential
  exponen<- inter + runif(1,-1,1)*exp(trend) + error
  
  #generate dependant variable and add trend
  flist <-data.frame(linear,quad, cubic,exponen)
  dep <- flist[,sample(c(1,2,3,4),1)]
  
  df<-data.frame(seq(1:n),dep)
  names(df)<-var.names
  return (df)
}

library(ggplot2)
library(splines)
library(MASS)
library(shiny)

shinyServer(function(input, output) {
  
  df<-reactive({
    input$generateButton
    generateData(input$points)
  })
  
  output$pointPlot <- renderPlot({
    
    # draw plot of time series data
    g <- ggplot(df(), aes(x=Instance, y=Variable)) + geom_point() + 
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"))
    
    sfit = input$fitType
    rse= "No smoother selected."
    switch(sfit, 
           none={
             g<-g + ggtitle("Data with no fit")
             rse = "No smoother selected."
           },
           lin={
             g<-g + ggtitle("Data with linear smoother") + stat_smooth(method="lm")
             rse= "Linear smoother selected."

           },
           p2={
             g<-g + ggtitle("Data with second degree polynom smoother") +
            stat_smooth(method="lm", formula = y ~ ns(x,2))
             rse="Plynomial 2-nd degree smoother selected."
           },
           p3={
             g<-g + ggtitle("Data with third degree polynom smoother") +
               stat_smooth(method="lm", formula = y ~ ns(x,3))
             rse="Plynomial 3-nd degree smoother selected."
           }
    )
    print(g)
    
    output$modelrse<-renderText(rse)

  })

})
