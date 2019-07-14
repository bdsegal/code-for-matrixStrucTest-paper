# simulations under white noise

library(MASS)
library(lavaan)
library(matrixStrucTest)
source("simFunctions.R")

# setup simulation quantities
pk <- c(5, 7, 9, 11)
pkCum <- cumsum(pk)
pkCumRev <- rev(cumsum(rev(pk)))

# simulations
mu <- rep(0, sum(pk)) 

group_list <- list()
for (i in 1:length(pk)){
  group_list[[i]] <- 
  (c(0,pkCum)[i]+1):c(0,pkCum)[i+1]
}

# number of MC resamples B and simulations N
B <- 1000
N <- 1000

# arrays for storing results ------------------------------

# t-stat 1-sided
simResultsT1 <- array(NA, dim = c(3,(1+length(pk)), N),
  dimnames = list(n = c(10,100,1000), 
    p = c("overall", 1:length(pk)),
    iter = 1:N
  )
)

# t-stat 2-sided
simResultsT2 <- array(NA, dim = c(3,(1+length(pk)), N),
  dimnames = list(n = c(10,100,1000), 
    p = c("overall", 1:length(pk)),
    iter = 1:N
  )
)

# Hubert's gamma one-sided
simResultsG1 <- array(NA, dim = c(3,(1+length(pk)), N),
  dimnames = list(n = c(10,100,1000), 
    p = c("overall", 1:length(pk)),
    iter = 1:N
  )
)

# Hubert's gamma two-sided
simResultsG2 <- array(NA, dim = c(3,(1+length(pk)), N),
  dimnames = list(n = c(10,100,1000), 
    p = c("overall", 1:length(pk)),
    iter = 1:N
  )
)

GOFstats <- c("chisq", "df", "pvalue", 
              "chisq.scaled", "df.scaled", "pvalue.scaled",
              "cfi", "tli", 
              "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue",
              "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "rmsea.pvalue.scaled",
              "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "rmsea.pvalue.robust")

simResultsCFA <- array(NA, dim = c(3, length(GOFstats), N),
  dimnames = list(n = c(10,100,1000), 
    p = GOFstats,
    iter = 1:N
  )
)

simResultsX2Pearson <- array(NA, dim = c(3, 3, N),
  dimnames = list(n = c(10,100,1000), 
    p = c("X2", "df", "pval"),
    iter = 1:N
  )
)

simResultsX2Spearman <- array(NA, dim = c(3, 3, N),
  dimnames = list(n = c(10,100,1000), 
    p = c("X2", "df", "pval"),
    iter = 1:N
  )
)

# setup model statement for CFA
letters2 <- c(letters, paste("z", 2:10, sep = ""))

B1 <- paste(letters2[group_list[[1]]], collapse = " + ")
B2 <- paste(letters2[group_list[[2]]], collapse = " + ")
B3 <- paste(letters2[group_list[[3]]], collapse = " + ")
B4 <- paste(letters2[group_list[[4]]], collapse = " + ")

model <- paste('B1 =~', B1, "\n",
               'B2 =~', B2, "\n",
               'B3 =~', B3, "\n",
               'B4 =~', B4, sep = "")

# simulations
for (power in 1:3){
  print(power)
  for (iter in 1:N){
    print(iter)
    try({

      # white noise covariance
      sigma <- array(rnorm(n = sum(pk) * sum(pk), mean= 0 , sd = 1), dim = c(sum(pk), sum(pk)))
       
      # Ensure that the covariance matrix positive definite
      sigma <- t(sigma) %*% sigma

      # True correlation matrix
      s <- diag(diag(sigma)^(-1/2))
      sigmaCor <- s %*% sigma %*% s

      # generate data and get correlation matrix
      y <- mvrnorm(n = 10^power, mu = mu, Sigma = sigma)
      yCat <- ordFun(y)
      A <- cor(yCat, method = "spearman")

      # permutation test
      out <- matrixStrucTest(A, group_list, B=B, absolute=TRUE)
      simResultsT1[power, , iter] <- c(out$pt_overall_one_sided, out$pt_multi_one_sided)
      simResultsT2[power, , iter] <- c(out$pt_overall_two_sided, out$pt_multi_two_sided)
      simResultsG1[power, , iter] <- c(out$pG_overall_one_sided, out$pG_multi_one_sided)
      simResultsG2[power, , iter] <- c(out$pG_overall_two_sided, out$pG_multi_two_sided)

      # prep data for pattern hypothesis test and CFA
      colnames(yCat) <- letters2[1:ncol(yCat)]
      yCatOrd <- as.data.frame(yCat)
      for (j in 1:ncol(yCatOrd)){
        yCatOrd[, j] <- as.ordered(yCatOrd[, j])
      }
      
      # pattern hypothesis testing
      simResultsX2Pearson[power, , iter]  <- X2Fun(yCat, group_list, corMethod = "pearson")
      simResultsX2Spearman[power, , iter] <- X2Fun(yCat, group_list, corMethod = "spearman")

      # CFA
      simResultsCFA[power, , iter] <- fitMeasures(cfa(model, data = yCatOrd), GOFstats)
    
      # make sure output is not retained into next round, in case next iteration fails
      rm(out)
    })

    save(simResultsT1, simResultsT2, simResultsG1, simResultsG2,
         simResultsX2Pearson, simResultsX2Spearman, simResultsCFA,
         file = "simWhiteNoise_1000_tG.Rdata")
  }
}

