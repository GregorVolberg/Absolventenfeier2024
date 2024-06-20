library(imager)

imgdir <- "C:/Users/LocalAdmin/Documents/A"
fnames <- dir(imgdir, '*.png')
fpaths <- paste0(imgdir, '/', fnames)
# 1:3 tier, 4:6 prof, 7:9 moebel

pics <- list(NULL)
for (k in 1:length(fnames)){
  pics[[k]] <- load.image(fpaths[k])
}

c <- load.image('cross.png')
trials <- sample(rep(1:9, 10))

x11()
par(oma=c(6,6,6,6))
for (j in 1:length(trials)){
  Sys.sleep(1+runif(1)/2)
  plot(pics[[trials[j]]], axes=F)
  Sys.sleep(1.5+runif(1)/2)
  plot(c, axes=F)
}
  

