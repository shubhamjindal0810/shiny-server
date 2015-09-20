
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

options(shiny.maxRequestSize = 9*1024^2)

shinyServer(function(input, output, session) {
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    library(data.table)   
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    write.csv( data.table(read.csv( inFile$datapath )), "~/shiny-server/data.csv")
    read.csv(inFile$datapath)
  })
  
  begin <- eventReactive(input$begin, {
    plotImage <- function( metadata, logo, img1, barcodeSKU, SKU, blank = 0,  plotLogo = 1 ){
     setwd('~/shiny-server')
	 plot( 1 : 2, type = 'n', yaxt = 'n', xaxt = 'n', ylab = '', xlab = '', bty = 'n' )#for no border
      if( blank == 0 ){
        text( 1, 1.77, metadata, pos = 4, cex = 0.75)
        if( plotLogo == 1) rasterImage( logo, 1.61, 1.67, 2.0 , 1.84 )
        rasterImage( img1, 0.94, 1.19, 2.06, 1.64 )
        text( 1.5, 1.12, paste( "AMAZON FNSKU :", barcodeSKU ), cex = 0.85 )
        #rasterImage( img2, 1.001, 1.1, 1.999, 1.3 )
        text( 1.5, 1.03, paste( "SKU:", SKU ) , cex = 0.85 )
        #box( )#lty = '1373', col = 'black' )
      }
    }
    library( data.table )
    library( rPython )
    library( png )
    pythonCode <- "~/shiny-server/python.py"
    pythonFunction <- "jindsh"
    
    tempImageName <- "temp.png"
    orderFile <- "~/shiny-server/data.csv"
    plotLogo = 1
    
    order <- data.table( read.csv( orderFile ) )
    code <- readChar( pythonCode, nchars = 100000 )
    python.exec( code )
    pdf( file = "test.pdf", onefile = TRUE, paper = "a4", bg = "white", width = 7.8, height = 11 )
    par( mfrow = c( 8, 3 ), mar = rep( 0.5, 4 ) )
    
    for ( i in 1 : nrow( order ) ){
      if( order$Quantity[ i ] > 0 ){
        barcodeSKU <- order$fnsku[ i ]
        SKU <- order$sku[ i ]
        python.call( pythonFunction, barcodeSKU, tempImageName )
        img1 <- readPNG( tempImageName )
        name <- order$product.name[ i ]
        gen <- "Imported by Jindal Trading Company, Hyderabad"
        mrp <- paste( "MRP: ", order$MRP[ i ], " INR", sep = "" )
        month <- paste( "Import Month : ", order$Import.Month[ i ], sep = "" )
        logoImage <- paste( order$Brand..Wizzit.JT.dbrand.None.[ i ], ".png", sep = "" )
        logo <- readPNG( logoImage )
        metadata <- paste( gen, mrp, name, month, sep = "\n" )
        for ( j in 1 : order$Quantity[ i ] ){
          plotImage( metadata, logo, img1, barcodeSKU, SKU, plotLogo = plotLogo )
        }
        k = 3 - j%%3
        for( l in 1 : k ){
          plotImage( metadata, logo, img1, barcodeSKU, SKU, blank = 1, plotLogo = plotLogo )
        }
      }
    }
    dev.off( )
    "Done"
    
  })
  
  output$status <- renderText({
    begin()
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() { 
      'stickers.pdf'
    },
    content = function(file) {
      file.copy('test.pdf', file)
    }
  )
  
})
