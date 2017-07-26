## Reading the command line arguments
commandline.args <- commandArgs( trailing = TRUE )



## Check the supplied arguments (a path to an image)
if ( length( commandline.args ) == 0 ){
  stop( "No image provided via the command line. Please supply either the name of an image in the current directory or full path to one in your system!" )
} else if ( length( commandline.args ) > 1 ){
  stop( "To many inputs! Please just supply one image!" )
} else {
  ## Check whether the input actually correspond to an existing image
  if ( length( grep( ".png", commandline.args ) ) != 0 ||
       length( grep( ".jpg", commandline.args ) ) != 0 ||
       length( grep( ".jpeg", commandline.args ) ) != 0 ){
    ## These checks are just for convenience. In principle the algorithm
    ## works with any file.
    ## Check whether the file actually exists
    if ( !file.exists( commandline.args ) ){
      stop( "Couldn't find the supplied file. Be sure to use the full path if it's not located in the current folder!" )
    } else {
      file.name <- commandline.args
    }
  } else {
    stop( "Please supply a file of type .jpeg, .jpg, or .png!" )
  }
}

## Install and load the require packages
if ( !( "ggplot2" %in% rownames( installed.packages() ) ) ){
  install.packages( "ggplot2",
                   repos = "http://ftp.gwdg.de/pub/misc/cran/" )
}
if ( !( "BMS" %in% rownames( installed.packages() ) ) ){
  install.packages( "BMS", repos = "http://ftp.gwdg.de/pub/misc/cran/" )
}
library( ggplot2 )
library( BMS )

## Invoke a bash command to convert the image into it's hexagonal
## representation.
system2( "xxd", args = paste0( "-p ", file.name, " > ",
                              file.name, ".hex" ) )

## Read the hexagonal representation into the R process
image.hexagonal <- read.csv( paste0( file.name, ".hex" ),
                            header = FALSE )
## Convert the hexagonal representation into a binary one
image.binary <- Reduce( c, apply( image.hexagonal, 1, hex2bin ) )

## Invoke exiftool via a system command in order to access the image's
## aspect ratio.
image.size.system <- system2( "exiftool",
                             args = paste( "-ImageSize", file.name ),
                             stdout = TRUE )
## Remove whitespaces
image.size.system.no.whitespaces <- gsub( " ", "", image.size.system )
## Extract the numerical values
image.size.system.second.part <-
  strsplit( image.size.system.no.whitespaces, split = ":" )[[ 1 ]][ 2 ]
image.size <- as.numeric( strsplit( image.size.system.second.part,
                                   split = "x" )[[ 1 ]] )

## Determine the correct aspect ratio. It should be the same as in the
## original image.
plot.size <- c( 
    round( sqrt( length( image.binary )* image.size[ 1 ]/
                 image.size[ 2 ] ) ),
    round( image.size[ 1 ]* image.size[ 2 ]/ image.size[ 1 ] ) )

## Now we will generate a heatmap from our data: we will span a x-y grid
## (number of pixels in the width and height) and fill it with the
## binary representation of our original image
plot.grid <- expand.grid( 1 : plot.size[ 1 ], 1 : plot.size[ 2 ] )
plot.heatmap <- data.frame(
    x = plot.grid[ , 2 ], y = plot.grid[ , 1 ], 
    data = image.binary[ 1 : nrow( plot.grid ) ] )

## For the plot I will use ggplot2.
## Check out the reference http://ggplot2.tidyverse.org/reference/
## for different colors or styles.
ggplot( data = plot.heatmap, aes( x = x, y = y, colour = data ) ) + 
                geom_tile( fill = "darkolivegreen3") + 
    scale_colour_gradientn( colours = c( "khaki1", "olivedrab4" ) ) +
    theme( axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none",
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_blank() )

## Save the resulting plot to disk
ggsave( "heatmap.png" , width = image.size[ 1 ]/ 10,
       height = image.size[ 2 ]/ 10 )
