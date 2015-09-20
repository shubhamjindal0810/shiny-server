############## Printing bar codes for Flipkart, Amazon, Snapdeal Fulfillment


#Dependencies:
# rPython
# data.table
# png

library( rPython )
library( data.table )

plotImage <- function( metadata, logo, img1, barcodeSKU, SKU, blank = 0,  plotLogo = 1 ){
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

printBarcode <- function( orderFile ){
  
  
  setwd( "C:/Users/Shubham/Documents/")
  library( data.table )
  library( rPython )
  library( png )
  pythonCode <- "python.py"
  pythonFunction <- "jindsh"
 
  tempImageName <- "temp.png"
  orderFile <- "data.csv"
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
}






flipkartSticker <- function( orderFile ){
  setwd( "C:/Users/Shubham/Documents/")
  library( data.table )
  library( png )
  
  tempImageName <- "temp.png"
  orderFile <- "flip.csv"
  plotLogo <- 1
  
  order <- data.table( read.csv( orderFile ) )
  pdf( file = "test.pdf", onefile = TRUE, paper = "a4", bg = "white", width = 7.8, height = 11 )
  par( mfrow = c( 8, 3 ), mar = rep( 0.5, 4 ) )
  
  
  for ( i in 1 : nrow( order ) ){
    if( order$Quantity[ i ] > 0 ){
      
      gen <- "Manufactured and imported from dbrand inc, Canada\nby Jindal Trading Company"
      SKU <- paste( "SKU: ", order$Seller.SKU.Id[ i ], sep = "" )
      fsn <- paste( "FSN: ", order$Flipkart.Serial.Number[ i ], sep = "" )
      modelID <- paste( "Model ID: ", order$Model.ID[ i ], sep = "" )
      #mrp <- paste( "MRP: ", order$MRP[ i ], " INR", sep = "" )
      mrp <- "MRP: 2999/- INR"
      color <- paste( "Color: ", order$Color[ i ], sep = "" )
      comp <- paste( "Compatibility: ", order$Compatibility[ i ], sep = "" )
     # month <- paste( "Import Month : ", order$Import.Month[ i ], sep = "" )
      month <- "Import month: Sep-15"
      name <- order$Product.Title[ i ]
     
      
      logoImage <- paste( "dbrand", ".png", sep = "" )
      logo <- readPNG( logoImage )
      
      img1 <- readPNG( "None.png" )
      
      metadata <- paste( gen, modelID, mrp, color, month, comp, sep = "\n" )
      for ( j in 1 : order$Quantity[ i ] ){
        plotFlipImage( metadata, logo, fsn, SKU, plotLogo = plotLogo )
      }
      k = 3 - j%%3
      #for( l in 1 : k ){
      #  plotFlipImage( metadata, logo, fsn, SKU, blank = 1, plotLogo = plotLogo )
      #}
    }
  }
  dev.off( )
}

plotFlipImage <- function( metadata, logo, barcodeSKU, SKU, blank = 0,  plotLogo = 1 ){
  plot( 1 : 2, type = 'n', yaxt = 'n', xaxt = 'n', ylab = '', xlab = '' )#, bty = 'n' )#for no border
  if( blank == 0 ){
    text( 0.95, 1.5, metadata, pos = 4, cex = 0.85)
    if( plotLogo == 1) rasterImage( logo, 1.61, 1.5, 2.0 , 1.67 )
    #rasterImage( img1, 0.94, 1.19, 2.06, 1.64 )
    text( 1.5, 1.18, paste( barcodeSKU ), cex = 0.95 )
    #rasterImage( img2, 1.001, 1.1, 1.999, 1.3 )
    text( 1.5, 1.03, paste( SKU ) , cex = 0.95 )
    #box( )#lty = '1373', col = 'black' )
  }
  if( blank == 1 ){
    text ( 1.5, 1.5, "//SKU CHANGE//", pos = 4, cex = 1 )
  }
}




uploadAmazon <- function( scanFile, linkFile = "order.csv", templateFile = "fba_template.csv", outputFile = "upload.txt" ){
  scan <- read.csv( scanFile )
  data <- data.table( fsn = scan$fsn )
  dataCount <- data.table( count( data, "fsn" ) )
  link <- data.table( read.csv( linkFile ) )
  setnames( link, "fsn", "FSN" )
  dataCount[ , SKU := link$sku[ which( as.character( link$FSN ) == fsn ) ] ]
  final <- data.table( MerchantSKU = dataCount$SKU, Quantity = dataCount$freq )
  final <- data.frame( final )
  template <- data.frame( read.csv( templateFile ), stringsAsFactors = FALSE )
  template[ , c( 1, 2 ) ] <- sapply( template[ , c( 1, 2 ) ],as.character )
  final[ ,1 ] <- sapply( final[ , 1 ], as.character )
  for ( i in 1 : nrow( final ) ){
    template[ ( 11 + i ), 1 : 2 ] <- final[ i, 1 : 2 ]
  }
  write.table( template, outputFile, row.names = FALSE, sep = "\t", eol = "\n", quote = FALSE )
}

mainFileTable <- read.csv( mainFile )

predictOrder <- function ( file ){
  install.packages( "data.table" )
  library( data.table )
  file = "C:/Users/Shubham/Downloads/205493774016675.txt"
  data <- data.table( read.csv( file, sep = "\t" ) )
  timestamp <- data$timestamp[ 1 ]
  data[, weeks.of.cover.min := min( as.factor( weeks.of.cover.t7 ), as.factor( weeks.of.cover.t30 ), as.factor( weeks.of.cover.t90 ), as.factor( weeks.of.cover.t180 ), as.factor( weeks.of.cover.t365 ) ) ]
  
  index <- which( as.character( data$weeks.of.cover.t7 ) == "Infinite" )
  data$weeks.of.cover.t7[ index ] <- rep( Inf, length( index ) )                 
}


























