source("fun.R")
require('magick')

sources <- c("normal_beta","normal","exponentials","beta")

img <- readPNG(system.file("img", "Rlogo.png", package="png"))

read_image <- function(source,idx){
  path <- paste0("img/plot_auc_",source,"_",two_digits(idx),".png")
  
  img <- image_read(path)
  
  return(img)
}

generate_mosaics <- function(x){
  top_row <- c(read_image(sources[1],x),
               read_image(sources[2],x))
  bottom_row <- c(read_image(sources[3],x),
                  read_image(sources[4],x))
  
  mosaic_plot <- image_append(c(image_append(top_row,stack = F),
                                image_append(bottom_row,stack = F)),
                              stack=T)
  
  image_write(mosaic_plot,
              path = paste0("img/mosaics/plot_auc_mosaic_",two_digits(x),".png"),
              format = "png")             
  print(paste0("image saved at img/mosaics/plot_auc_mosaic_",x,".png"))
}
for (i in 1:10){
  generate_mosaics(i)  
}
setwd("img/mosaics")
system("convert -delay 50 *.png animation.gif")
