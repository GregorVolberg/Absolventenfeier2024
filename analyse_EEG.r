library(tidyverse)
library(signal)
library(spectral)

chanleft  <- 3
chanright <- 1
chanzentral <- 2


eeg <- read_delim("./data/eegAbsolventenfeier-20240620-153558.csv", col_names = F) %>%
  select(2:9) %>%
  as.matrix()

marker <- read_delim("./data/eegAbsolventenfeier-20240620-153558.csv", col_names = F) %>%
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
tfr1<- matrix(dim(markerindex)[1]*(251+125), nrow=dim(markerindex)[1], (251+125))
tfr2<- matrix(dim(markerindex)[1]*(251+125), nrow=dim(markerindex)[1], (251+125))

for (n in 1:dim(markerindex)[1]){
  bsl1[n] <- mean(eegfilt1[(markerindex[n,1]-125):markerindex[n,1]])
  bsl2[n] <- mean(eegfilt2[(markerindex[n,1]-125):markerindex[n,1]])
  tfr1[n, ] <- as.vector(eegfilt1[(markerindex[n,1]-125):(markerindex[n,1]+250)])
  tfr2[n, ]  <- as.vector(eegfilt2[(markerindex[n,1]-125):(markerindex[n,1]+250)])
}


#tfrdiff <- abs(envelope(tfr1 - bsl1)) - abs(envelope(tfr2 - bsl2))

tfrdiff <- abs(envelope(tfr1)) - abs(envelope(tfr2 - bsl2))

meancon = matrix(376*9, 9, 376)
for (con in 1:9){
  meancon[con,] <- apply(tfrdiff[(markerindex[,2] == con),], 2, mean)
}

# left minus right
# pos means left higher than right, ie right more activity than left, ie negative emotion
# rescale to have pos <- pos emotion, neg <- neg emotion
meancon <- meancon *-1

mmed <- function(x,n=5){runmed(x,n)} #Median
wn=51
m1 = mmed(apply(meancon[1:3, ], 2, mean),wn)
m2 = mmed(apply(meancon[4:6, ], 2, mean), wn)
m3 = mmed(apply(meancon[7:9, ], 2, mean), wn)

plot(m1,
     type='l', col='black', lwd=2, ylim = c(-2,2))
lines(m2,
      type='l', col = 'red', lwd=2)
lines(m3,
      type='l', col = 'blue', lwd=2)
#1:3 tier, 4:6 prof, 7:9 moebel
#was passiert in ersten 200 ms 
start1 = 150; stop1 = 200
barplot(c(m1[stop1]- m1[start1], 
        m2[stop1]- m2[start1],
        m3[stop1] - m3[start1]), ylim = c(-0.8, 0.8))

# 200 bis 600
start2 = 200; stop2 = 300
barplot(c(m1[stop2]- m1[start2], 
          m2[stop2]- m2[start2],
          m3[stop2] - m3[start2]), ylim = c(-0.8, 0.8))

# 600 bis 800
start3 = 300; stop3 = 350
barplot(c(m1[stop3]- m1[start3], 
          m2[stop3]- m2[start3],
          m3[stop3] - m3[start3]), ylim = c(-0.8, 0.8))

mmeancon= meancon
for (w in 1:9){
mmeancon[w,] = mmed(meancon[w, ], wn)}

srt=sort(apply(mmeancon[,350:376], 1, mean))
barplot(srt$x[1:3])
srt$ix # 3 :2:1, fisch, prof, hamster


"fisch.png"   "hamster.png" "katze.png"   "prof1.png"  
"prof2.png"   "prof3.png"   "schrank.png" "stuhl.png"   "tisch.png"  


matplot(t(meancon[1:3, ]),
     type='l', col='black', lwd=2, ylim = c(-2,2))

tst=apply(meancon[1:3, ], 2, mean)
plot(mmed(tst, 50), type='l')(abs(envelope(tfr1)))[1,], type='l')
plot(tfr1[1,])



## alternativ
meancon1 = matrix(376*9, 9, 376)
meancon2 = matrix(376*9, 9, 376)
for (con in 1:9){
  meancon1[con,] <- apply(tfr1[(markerindex[,2] == con),], 2, mean)
  meancon2[con,] <- apply(tfr2[(markerindex[,2] == con),], 2, mean)
}
meancon1 = meancon1 - mean(meancon1[,1:125])
meancon2 = meancon2 - mean(meancon2[,1:125])
m1=meancon1
m2 = meancon2
for (m in 1:9){
m1[m,] = mmed(meancon1[m,], 50)
m2[m,] = mmed(meancon2[m,], 50)
}

matplot(t(m1), type='l')
        




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