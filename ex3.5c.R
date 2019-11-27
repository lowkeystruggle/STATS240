problem3data.stocks <- read.csv("m_logret_10stocks.txt", sep="\t")
problem3data.market <- read.csv("m_sp500ret_3mtcm.txt", sep="\t")
problem3data.stocks <- problem3data.stocks[1:156,]

muVector <- colMeans(problem3data.stocks[-c(1)])
print(muVector)
covMatrix <- cov(problem3data.stocks[-c(1)])
print(covMatrix)
covMatrixInverse <- solve(covMatrix)
onesVector <- rep(1,10)

valueA <- muVector%*% covMatrixInverse %*% onesVector
valueB <- muVector %*% covMatrixInverse %*% muVector
valueC <- onesVector %*% covMatrixInverse %*% onesVector
valueD <- valueB %*% valueC - valueA %*% valueA
valueA <- valueA[1][1]
valueB <- valueB[1][1]
valueC <- valueC[1][1]
valueD <- valueD[1][1]
effWeights <- (valueB * covMatrixInverse %*% onesVector 
               - valueA * covMatrixInverse %*% muVector 
               + 0.003* (valueC * covMatrixInverse %*% muVector 
                       - valueA * covMatrixInverse %*% onesVector))/valueD
print(effWeights)
problem3data.stocksOnly <- problem3data.stocks[-c(1)]
michaudWeights <- rep(0,10)
for (i in 1:500)
{
  tempSamples <- problem3data.stocksOnly[c(sample(156, replace = TRUE)), ]
  tempMuVector <- colMeans(tempSamples)
  tempCovMatrix <- cov(tempSamples)
  tempCovMatrixInverse <- solve(tempCovMatrix)
  tempA <- tempMuVector %*% tempCovMatrixInverse %*% onesVector
  tempB <- tempMuVector %*% tempCovMatrixInverse %*% tempMuVector
  tempC <- onesVector %*% tempCovMatrixInverse %*% onesVector
  tempA <- tempA[1][1]
  tempB <- tempB[1][1]
  tempC <- tempC[1][1]
  tempD <- tempB * tempC - tempA * tempA
  michaudWeights <- michaudWeights + 
    (tempB * tempCovMatrixInverse %*% onesVector 
    - tempA * tempCovMatrixInverse %*% muVector 
    + 0.003* (tempC * tempCovMatrixInverse %*% muVector 
    - tempA * tempCovMatrixInverse %*% onesVector))/tempD
}

michaudWeights <- michaudWeights/500
print(michaudWeights)
