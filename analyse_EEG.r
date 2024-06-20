library(tidyverse)
library(signal)
library(spectral)

chanleft  <- 3
chanright <- 1
chanzentral <- 2


eeg <- read_delim("eegAbsolventenfeier-20240620-122950.csv", col_names = F) %>%
  select(2:9) %>%
  as.matrix()

marker <- read_delim("eegAbsolventenfeier-20240620-122950.csv", col_names = F) %>%
  select(24) %>%
  as.matrix()

markerindex <- t(rbind(which(marker !=0), marker[which(marker!=0)]))  


# filter
fs <- 250
forder <- 6
coef <- butter(forder, c(8, 12) / (fs/2), 'pass')
eegfilt1 <- filter(coef, eeg[,chanleft])
eegfilt2 <- filter(coef, eeg[,chanright])

bsl1 <- NULL
bsl2 <- NULL
tfr1<- matrix(dim(markerindex)[1]*251, nrow=dim(markerindex)[1], 251)
tfr2<- matrix(dim(markerindex)[1]*251, nrow=dim(markerindex)[1], 251)

for (n in 1:dim(markerindex)[1]){
  bsl1[n] <- mean(eegfilt1[(markerindex[n,1]-125):markerindex[n,1]])
  bsl2[n] <- mean(eegfilt2[(markerindex[n,1]-125):markerindex[n,1]])
  tfr1[n, ] <- as.vector(eegfilt1[markerindex[n,1]:(markerindex[n,1]+250)])
  tfr2[n, ]  <- as.vector(eegfilt2[markerindex[n,1]:(markerindex[n,1]+250)])
}

tfrdiff <- abs(envelope(tfr1 - bsl1)) - abs(envelope(tfr2 - bsl2))

meancon = matrix(251*9, 9, 251)
for (con in 1:9){
  meancon[con,] <- apply(tfrdiff[markerindex[,2] == con,], 2, mean)
}



hilbert(meancon[1,])

df <- as.tibble(markerindex) %>%
      mutate(bsl1      = mean(eegfilt1[(indx-125):indx]))

length(eegfilt1)
markerindex-125:markerindex
indx      = markerindex[,1]
df <- data.frame(indx,
                 condition = markerindex[,2],
                 bsl1      = mean(eegfilt1[(indx-125):indx]))
            
, markerindex



  
test1 = filter(coef, eeg[10000:10500,2])

plot(eeg[10000:10500,2], type='l')
plot(test1, type='l', add=T)

fir1(10, c(8, 12) / (fs / 2), "pass")
order <- 5
fcutlow <- 8
fcuthigh <- 12
[b,a] <- butter(order,[fcutlow,fcuthigh]/(fs/2),'bandpass');
  filtsig=filter(b,a,inputsig);  %filtered signal  