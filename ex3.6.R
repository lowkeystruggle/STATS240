problem3data.stocks <- read.csv("m_logret_10stocks.txt", sep="\t")
problem3data.market <- read.csv("m_sp500ret_3mtcm.txt", sep="\t")
problem3data.stocks <- problem3data.stocks[1:156,]
problem3data.stocksOnly <- problem3data.stocks[-c(1)]
bootstrapStats <- matrix(0L,500,4)
tmp <- matrix(0L,156, 10)
alphaHat <- c()
riskFreeRate = sum(problem3data.market$sp500)

for (j in 1:10)
{
  for (i in 1:500)
  {
    tempSample <- problem3data.stocksOnly[c(sample(156, replace = TRUE)), ]
    tempSample <- as.matrix.data.frame(tempSample[j])
    lm_temp <- lm(tempSample~problem3data.market$sp500)
    tempStockStDev <- sd(tempSample)
    tempPortfRet <- sum(tempSample[j])
    bootstrapStats[i,1] = unname(lm_temp$coefficients[1]) #alpha
    bootstrapStats[i,2] = unname(lm_temp$coefficients[2]) #beta
    bootstrapStats[i,3] = (tempPortfRet - riskFreeRate)/tempStockStDev #calc of Sharpe
    bootstrapStats[i,4] = (tempPortfRet - riskFreeRate)/bootstrapStats[i,2]
    
  }
  cat("\n \n \n Company: ", colnames(problem3data.stocksOnly[j]), "\n \n ")
  cat("Std Error of alpha: ", format(sd(bootstrapStats[,c(1)])/sqrt(500), digits =2),"\n")
  lm_temp <- lm(as.matrix.data.frame(problem3data.stocksOnly[j])~problem3data.market$sp500)
  alphaHat <- c(alphaHat, lm_temp$coefficients[1])
  tmp[,c(j)] <- lm_temp$residuals
  print(summary(lm_temp))
  cat("T value of alpha: ", format(mean(bootstrapStats[,c(1)])/(sd(bootstrapStats[,c(1)])/sqrt(500)), digits =2), "\n")
  #cat("mean: ", mean(bootstrapStats[,c(1)]), "\n")
  #cat("sd: ", sd(bootstrapStats[,c(1)]), "\n")
  #cat("sqrt(n): ", sqrt(500), "\n")
  cat("Std Error of beta: ", format(sd(bootstrapStats[,c(2)])/sqrt(500), digits =2),"\n")
  cat("Std Error of Sharpe: ", format(sd(bootstrapStats[,c(3)])/sqrt(500), digits =2),"\n")
  cat("Std Error of Treynor: ", format(sd(bootstrapStats[,c(4)])/sqrt(500), digits =2), "\n")
}

########## F-stat
n<-156;  q<-10
#alphahat<-c(lm.Am$co[1], lm.C$co[1], lm.Exx$co[1], lm.GM$co[1], lm.In$co[1], 	lm.Pf$co[1])
#tmp <- cbind(lm.Am$re, lm.C$re, lm.Exx$re, lm.GM$re, lm.In$re, lm.Pf$re)
bSigmahat <- t(tmp)%*%tmp/n
nu <- t(alphaHat) %*% solve(bSigmahat) %*% alphaHat
de <- 1 + mean(problem3data.market$sp500)^2/ (var(problem3data.market$sp500)*(n-1)/n)
stat <- (n-q-1)/q * nu / de
cat("F statistic: ", stat, "\n")
cat("Critical F value: ", qf(0.90, 10, n-q-1), "\n")