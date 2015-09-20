
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  titlePanel("Uploading Files"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'First:  Upload a .csv file',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                )
      ),
     # numericInput("n", "N:", min = 0, max = 100, value = 50),
      br(),
      p("Second: Click on the below button and wait untill you see DONE " ),
      actionButton("begin", "Start Generating Stickers"),
      verbatimTextOutput("status"),
     p("Third: Once you see DONE, click on the DOWNLOAD button " ),
      downloadButton('downloadData', 'Download')
    ),
    mainPanel(
      tableOutput('contents')
      
    )
  )
))
