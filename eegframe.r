library(brainflow)


## full example, dummy board
Id      <- brainflow_python$BoardIds$SYNTHETIC_BOARD 
params  <- brainflow_python$BrainFlowInputParams()   
params$serial_port <- "COM3" # subject to change
myboard <- brainflow_python$BoardShim(Id, params) 

myboard$prepare_session() # start session
myboard$add_streamer('file://eegAbsolventenfeier.csv:w')
myboard$start_stream()    # start stream


myboard$insert_marker(1); # only works with active board

myboard$stop_stream()     # stop stream
myboard$release_session() # end session
