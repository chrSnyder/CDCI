#Author: Nick Fisk
#Author: Katie Lewis
#Rev-his V.1.0
#server.R is responsible for both the communication protocol
# and the calling fuctions to perform the heavy lifting of the application,
# such as rendering plots, generating pdfs, and performing statistical analysis.

  
  
  
library(shiny)
plotType<-function(x,type){
  if(type[1]=="Binary by Q"){
    type=hist(x,xlab="# Correct", breaks=23,freq=T,main="Histogram of Correct Answers to CDCI")
  }
  if(type[1]=="Partial Credit by Q"){
    type=barplot(x)
  }
  if(type[1]=="Binary by Q (density)"){
    type=hist(x,xlab="# Correct", breaks=23,freq=F,main="Histogram of Correct Answers to CDCI with Nonparametric Kernel Density")
  }
  if(type[1]=="Boxplot"){
    type=boxplot(x)
  }
  if(type[1]=="Binary by Q (normal distribution)"){
    type=hist(x,xlab="# Correct", breaks=23,freq=F,main="Histogram of Correct Answers to CDCI with Normal Curve Density")
  }
  if(type[1]=="Binary by Q (compare normal and kernel densities)"){
    type=hist(x,xlab="# Correct", breaks=23,freq=F,main="Histogram of Correct Answers Comparing Observed Density to Normal Curve")
  }
  #switch(type,"Binary by Q"=hist(x,xlab="# Correct"),"Partial Credit by Q"=barplot(x),"Boxplot"=boxplot(x))
  return(type)
}
include<-function(doInclude){
  if(doInclude=="Ignore"){
    doInclude<-FALSE
  }
  else{
    doInclude=TRUE
  }
  }
shinyServer(function(input,output){
  sliderValues<-reactive({
    data.frame(Name=c("startInd"),Value=as.character(c(input$startInd)),stringsAsFactors = FALSE)
    })
  output$contents<-renderTable({
    inFile<-input$file1
    if(is.null(inFile)){
      return (NULL)
      }
    read.csv(inFile$datapath,header=TRUE,sep= ",",
             quote=input$quote)
    
  })
  output$Index<-renderTable({sliderValues()})
  output$doZeros<-reactive({data.frame(Name=c("doZeros"),Value=as.logical(c(unlist(include(input$doZeros))[1])))})
  output$plott<-renderPlot({
    inFile<-input$file1
    if(is.null(inFile)){
      return (NULL)
    }
    inp<-read.csv(inFile$datapath,header=TRUE,quote=input$quote,sep=',')
    plotType(unlist(inp[2]),input$plot)
    if(input$plot=="Binary by Q (density)"){
      lines(density(unlist(inp[2]),from=0, to=max(unlist(inp[2]))),col="darkred")
    }
    if(input$plot=="Binary by Q (normal distribution)"){
      g=unlist(inp[2])
      m<-mean(g)
      std<-sqrt(var(g))
      curve(dnorm(x,mean=m,sd=std),col="darkblue",lwd=2,add=TRUE)
    }
    if(input$plot=="Binary by Q (compare normal and kernel densities)"){
      g=unlist(inp[2])
      m<-mean(g)
      std<-sqrt(var(g))
      curve(dnorm(x,mean=m,sd=std),col="darkblue",lwd=2,add=TRUE)
      lines(density(unlist(inp[2]),from=0, to=max(unlist(inp[2]))),col="darkred")

    }
    })
    output$downloadData<-downloadHandler(
      filename = function(){
        paste0(input$plot,'-',Sys.Date(),'.pdf')
      },
      content=function(con){
        inFile2<-input$file1
        if(is.null(inFile2)){
          return (NULL)
        }
        inp<-read.csv(inFile2$datapath,header=TRUE,quote=input$quote,sep=',')
        pdf(file="file.pdf")
        print(plotType(unlist(inp[2]),input$plot))
        dev.off()
      }
    )
    output$downloadReport<-downloadHandler(
        filename="temp_.csv",
        content=function(con){
          write.csv(data,con)
        }
      )
})

