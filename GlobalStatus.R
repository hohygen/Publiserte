#Plots the global temperaturedevelopment of the current year and a prognosis for the rest of the year based on the assumption that historic 
developments of global temperature describes the possible outcomes of this year. The plot is a bit strange since whats plotted in month 1 is january data, 
month two is average of january and february, month three january, february, and march data ending in month 12 with yearly averages.

GStatus <- function(Y=2016,M=3){ #Use Y for the year of yor analysis, and M for the month of interrest
  GData <- read.csv("http://data.giss.nasa.gov/gistemp/tabledata_v3/GLB.Ts+dSST.csv",na.strings = c("***","****"),colClasses = "numeric")
  D2 <- GData[,1:2]
  for (n in 2:12){
    print(n)
    print(dim(D2))
    D2 <- cbind(D2,
                rowMeans(GData[,2:(n+1)]))
    print(dim(D2))
  }
  plot(1:12,D2[D2[,1]==Y,2:13]/100,type="l",ylim=c(-1,1.5),xlab="Måned",ylab="Temperaturavvik",main="NASA - GISS")
  NY <- length(D2[,1])
  for (n in 1:NY){
    lines(c(1:12),D2[n,2:13]/100,col="gray")
  }
  
  lines(1:12,D2[D2[,1]==(Y-1),2:13]/100,col="blue",lwd=2)
  D3 <- D2
  NY <- length(D3[,1])
  for (n in 1:NY){
    D3[n,c(M:12)+1]<-D3[n,c(M:12)+1]-(D3[n,M+1]-D3[NY,M+1])
    lines(c(M:12),D3[n,c(M:12)+1]/100,col="orange")
  }
  lines(1:12,D2[D2[,1]==Y,2:13]/100,col="red",lwd=2)
  D3
}
