#Author: Nick Fisk
#Rev-his: V. 0.1.2
#UI.R: Part of the CDCI-SHINE. Contructs an user interface to the 
# web application. Sliders, buttons, file uploads, and downloads are
# all handled within. ui.R also dynamically informs the server to new
# user input.  server.R is responsible for both the communication protocol
# and the calling fuctions to perform the heavy lifting of the application,
# such as rendering plots, generating pdfs, and performing statistical analysis. 
library(shiny)
shinyUI(fluidPage(
titlePanel("Welcome to CDCI-SHiNE"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1','Upload CDCI results in CSV Format',accept=c(
        'text/csv','text/comma-separated-values,text/plain','.csv'
      )),
      tags$hr(),
      sliderInput("startInd","Column with 1st CDCI Question:",
                  min=1,max=50,value=1),
      radioButtons('doZeros','How to handle zeros',c("Ignore","Include"))
      ,
      radioButtons('quote','Quote',c(None='','Double'='"','Single'="'"),'"')
    ,
     radioButtons('plot','Plot',c("Binary by Q", "Binary by Q (density)","Binary by Q (normal distribution)","Partial Credit by Q", "Boxplot","Binary by Q (compare normal and kernel densities)"))
    ,
    downloadLink('downloadData','Download Current Selection |')
    ,
    downloadLink('downloadReport', 'Download Whole Report')
    )
    ,
    mainPanel(plotOutput('plott'),tableOutput('contents'),tableOutput("Index"),textOutput('doZeros')
    )
    )
))
