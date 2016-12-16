#Plots the global temperaturedevelopment of the current year and a prognosis for the rest of the year based on the assumption that historic 
#developments of global temperature describes the possible outcomes of this year. The plot is a bit strange since whats plotted in month 1 is january data, 
#month two is average of january and february, month three january, february, and march data ending in month 12 with yearly averages.

GStatus <- function(Y=2016,M=3){
  GData <- read.csv("http://data.giss.nasa.gov/gistemp/tabledata_v3/GLB.Ts+dSST.csv",na.strings = c("***","****"),colClasses = "numeric", skip=1)
  print(GData)
  D2 <- GData[,1:2]
  for (n in 2:12){
    #print(n)
    #print(dim(D2))
    D2 <- cbind(D2,
                rowMeans(GData[,2:(n+1)]))
    #print(dim(D2))
  }
#  plot(1:12,D2[D2[,1]==Y,2:13]/100,type="l",ylim=c(-1,1.5),xlab="Måned",ylab="Temperaturavvik",main="NASA - GISS")
  plot(1:12,D2[D2[,1]==Y,2:13],type="l",ylim=c(-1,1.5),xlab="Måned",ylab="Temperaturavvik",main="NASA - GISS")
  NY <- length(D2[,1])
  for (n in 1:NY){
#    lines(c(1:12),D2[n,2:13]/100,col="gray")
    lines(c(1:12),D2[n,2:13],col="gray")
  }

#  lines(1:12,D2[D2[,1]==(Y-1),2:13]/100,col="blue",lwd=2)
  lines(1:12,D2[D2[,1]==(Y-1),2:13],col="blue",lwd=2)
  D3 <- D2
  NY <- length(D3[,1])
  for (n in 1:NY){
    D3[n,c(M:12)+1]<-D3[n,c(M:12)+1]-(D3[n,M+1]-D3[NY,M+1])
#    lines(c(M:12),D3[n,c(M:12)+1]/100,col="orange")
    lines(c(M:12),D3[n,c(M:12)+1],col="orange")
  }
#  lines(1:12,D2[D2[,1]==Y,2:13]/100,col="red",lwd=2)
  lines(1:12,D2[D2[,1]==Y,2:13],col="red",lwd=2)
  D3
}

GStatus.UAV <- function(Y=2016,M=11){
  Rader <- 1 + 12*(Y-1979)+M
  Data <- read.table("http://www.nsstc.uah.edu/data/msu/v6.0beta/tlt/uahncdc_lt_6.0beta5.txt",header=T,nrows=Rader)
  GData <- c()
  for (Aar in 1979:(Y-1)){
    Aaretstall <- Aar
    for(n in 1:12){
      #print(Data[Data[,1]==Aar&Data[,2]==n,3])
      Aaretstall <- c(Aaretstall,Data[Data[,1]==Aar&Data[,2]==n,3])
      #print(Aaretstall)
    }
    GData <- rbind(GData,Aaretstall)
  }
  Aaretstall <- Y
  for(n in 1:M){
    #print(Data[Data[,1]==Aar&Data[,2]==n,3])
    Aaretstall <- c(Aaretstall,Data[Data[,1]==Y&Data[,2]==n,3])
    #print(Aaretstall)
  }
  Aaretstall <- c(Aaretstall,array(NA,(12-M)))
  GData <- rbind(GData,c(Aaretstall))
  GDD <- length(GData[,1])
  GData[GDD,(M+2):13] <- NA
  print(GData)
  #GData[Gdata[,1]==Y, GData[Gdata[,1]==Y,2:13]>10] <- NA
  D2 <- GData[,1:2]
  for (n in 2:12){
    D2 <- cbind(D2,
                rowMeans(GData[,2:(n+1)]))
    #print(dim(D2))
  }
  #  plot(1:12,D2[D2[,1]==Y,2:13]/100,type="l",ylim=c(-1,1.5),xlab="Måned",ylab="Temperaturavvik",main="NASA - GISS")
  plot(1:12,D2[D2[,1]==Y,2:13],type="l",ylim=c(-1,1.5),xlab="Måned",ylab="Temperaturavvik",main="UAV",sub="Normal fra datasettet")
  NY <- length(D2[,1])
  for (n in 1:NY){
    #    lines(c(1:12),D2[n,2:13]/100,col="gray")
    lines(c(1:12),D2[n,2:13],col="gray")
  }

  #  lines(1:12,D2[D2[,1]==(Y-1),2:13]/100,col="blue",lwd=2)
  lines(1:12,D2[D2[,1]==(Y-1),2:13],col="blue",lwd=2)
  D3 <- D2
  NY <- length(D3[,1])
  for (n in 1:NY){
    D3[n,c(M:12)+1]<-D3[n,c(M:12)+1]-(D3[n,M+1]-D3[NY,M+1])
    #    lines(c(M:12),D3[n,c(M:12)+1]/100,col="orange")
    lines(c(M:12),D3[n,c(M:12)+1],col="orange")
  }
  #  lines(1:12,D2[D2[,1]==Y,2:13]/100,col="red",lwd=2)
  lines(1:12,D2[D2[,1]==Y,2:13],col="red",lwd=2)
  D3
}
