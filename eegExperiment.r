library(imager)
library(brainflow)

# set up experiment
imgdir <- "./"
fnames <- dir(imgdir, '*.png')
fpaths <- paste0(imgdir, fnames) # 1:3 tier, 4:6 prof, 7:9 moebel

pics <- list(NULL)
for (k in 1:length(fnames)){
  pics[[k]] <- load.image(fpaths[k])
}

c <- load.image('cross.png')
trials <- sample(rep(1:9, 15))

# set up eeg
csvname <- paste0('file://eegAbsolventenfeier-', 
                  format(Sys.time(), "%Y%m%d-%H%M%S"),
                  '.csv:w')

Id      <- brainflow_python$BoardIds$CYTON_BOARD 
params  <- brainflow_python$BrainFlowInputParams()   
params$serial_port <- "COM3" # subject to change
myboard <- brainflow_python$BoardShim(Id, params) 
myboard$prepare_session() # start session
myboard$add_streamer(csvname)
myboard$start_stream()    # start stream

x11()
Sys.sleep(5)
par(oma=c(6,6,6,6))
for (j in 1:length(trials)){
  Sys.sleep(1+runif(1)/2)
  plot(pics[[trials[j]]], axes=F)
  myboard$insert_marker(trials[j]); # only works with active board
  
  Sys.sleep(1.5+runif(1)/2)
  plot(c, axes=F)
}

myboard$stop_stream()     # stop stream
myboard$release_session() # end session
dev.off()

