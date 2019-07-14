# simulations under true CFA model

library(MASS)
library(lavaan)
library(matrixStrucTest)
source("simFunctions.R")

# specify population model and CFA models to fit
populationModel <- 'B1 =~ a + 2*b + 1.5*c + 0.5*d + e
                    B2 =~ f + g + 0.4*h + 0.75*i + 2*j + 0.5*k + l
                    B3 =~ m + 0.5*n + o + 1.25*p + q + 3*r + s + 0.4*t + u
                    B4 =~ 1.25*v + w + 0.8*x + y + z + 0.4*z2 + z3 + 0.6*z4 + z5 + z6 + z7'

modelCorrect <- 'B1 =~ a + b + c + d + e

                 B2 =~ f + g + h + i + j + k + l
                 B3 =~ m + n + o + p + q + r + s + t + u
                 B4 =~ v + w + x + y + z + z2 + z3 + z4 + z5 + z6 + z7'

# setup simulation quantities
pk <- c(5, 7, 9, 11)
pkCum <- cumsum(pk)
pkCumRev <- rev(cumsum(rev(pk)))

group_list <- list()
for (i in 1:length(pk)){
  group_list[[i]] <- (c(0, pkCum)[i] + 1):c(0, pkCum)[i + 1]
}

# number of MC resamples B and simulations N
B <- 10000
N <- 1000

# sample size
n <- c(10, 50, 100, 1000)

GOFstats <- c("chisq", "df", "pvalue",
              "cfi", "tli", "srmr",
              "rmsea", "rmsea.ci.lower", "rmsea.ci.upper")

# arrays for storing results ------------------------------

# t-stat 1-sided
simResultsT1 <- array(NA, dim = c(length(n), (1+length(pk)), N),
  dimnames = list(n = n, 
    p = c("overall", 1:length(pk)),
    iter = 1:N
  )
)

# t-stat 2-sided
simResultsT2 <- array(NA, dim = c(length(n), (1+length(pk)), N),
  dimnames = list(n = n, 
    p = c("overall", 1:length(pk)),
    iter = 1:N
  )
)

# Hubert's gamma 1-sided
simResultsG1 <- array(NA, dim = c(length(n), (1+length(pk)), N),
  dimnames = list(n = n, 
    p = c("overall", 1:length(pk)),
    iter = 1:N
  )
)

# Hubert's gamma 2-sided
simResultsG2 <- array(NA, dim = c(length(n), (1+length(pk)), N),
  dimnames = list(n = n, 
    p = c("overall", 1:length(pk)),
    iter = 1:N
  )
)

# t-stat 1-sided
simResultsT1_discrete <- array(NA, dim = c(length(n), (1+length(pk)), N),
  dimnames = list(n = n, 
    p = c("overall", 1:length(pk)),
    iter = 1:N
  )
)

# t-stat 2-sided
simResultsT2_discrete <- array(NA, dim = c(length(n), (1+length(pk)), N),
  dimnames = list(n = n, 
    p = c("overall", 1:length(pk)),
    iter = 1:N
  )
)

# Hubert's gamma 1-sided
simResultsG1_discrete <- array(NA, dim = c(length(n), (1+length(pk)), N),
  dimnames = list(n = n, 
    p = c("overall", 1:length(pk)),
    iter = 1:N
  )
)

# Hubert's gamma 2-sided
simResultsG2_discrete <- array(NA, dim = c(length(n), (1+length(pk)), N),
  dimnames = list(n = n, 
    p = c("overall", 1:length(pk)),
    iter = 1:N
  )
)

simResultsCFA <- array(NA, dim = c(length(n), length(GOFstats), N),
  dimnames = list(n = n, 
    p = GOFstats,
    iter = 1:N))

simResultsCFA_discrete <- array(NA, dim = c(length(n), length(GOFstats), N),
  dimnames = list(n = n, 
    p = GOFstats,
    iter = 1:N))

simResultsX2Pearson <- array(NA, dim = c(length(n), 3, N),
  dimnames = list(n = n, 
    p = c("X2", "df", "pval"),
    iter = 1:N))

simResultsX2Spearman <- array(NA, dim = c(length(n), 3, N),
  dimnames = list(n = n, 
    p = c("X2", "df", "pval"),
    iter = 1:N))

simResultsX2Pearson_discrete <- array(NA, dim = c(length(n), 3, N),
  dimnames = list(n = n, 
    p = c("X2", "df", "pval"),
    iter = 1:N))

simResultsX2Spearman_discrete <- array(NA, dim = c(length(n), 3, N),
  dimnames = list(n = n, 
    p = c("X2", "df", "pval"),
    iter = 1:N))

# simulations
for (ni in 1:4){
  for (iter in 1:N){
    print(paste0("n = ", n[ni], ", iter = ", iter))

    Y <- simulateData(populationModel, sample.nobs=n[ni])
    A <- cor(Y, method = "spearman")

    Y_discrete <- ordFun(Y)
    colnames(Y_discrete) <- colnames(Y)
    A_discrete <- cor(Y_discrete, method = "spearman")

    try({

      # permutation test
      out <- matrixStrucTest(A, group_list, B = B, absolute = TRUE)
      simResultsT1[ni, , iter] <- c(out$pt_overall_one_sided, out$pt_multi_one_sided)
      simResultsT2[ni, , iter] <- c(out$pt_overall_two_sided, out$pt_multi_two_sided)
      simResultsG1[ni, , iter] <- c(out$pG_overall_one_sided, out$pG_multi_one_sided)
      simResultsG2[ni, , iter] <- c(out$pG_overall_two_sided, out$pG_multi_two_sided)

      # pattern hypothesis testing
      simResultsX2Pearson[ni, , iter]  <- X2Fun(Y, group_list, corMethod = "pearson")
      simResultsX2Spearman[ni, , iter] <- X2Fun(Y, group_list, corMethod = "spearman")

      # CFA
      simResultsCFA[ni, , iter] <- fitMeasures(cfa(modelCorrect, data = Y), GOFstats)

      # make sure output is not retained into next round, in case next iteration fails
      rm(out)
      })
    
    try({
      # discretized ---------------------------------------

      # permutation test
      out <- matrixStrucTest(A_discrete, group_list, B = B, absolute = TRUE)
      simResultsT1_discrete[ni, , iter] <- c(out$pt_overall_one_sided, out$pt_multi_one_sided)
      simResultsT2_discrete[ni, , iter] <- c(out$pt_overall_two_sided, out$pt_multi_two_sided)
      simResultsG1_discrete[ni, , iter] <- c(out$pG_overall_one_sided, out$pG_multi_one_sided)
      simResultsG2_discrete[ni, , iter] <- c(out$pG_overall_two_sided, out$pG_multi_two_sided)

      # pattern hypothesis testing
      simResultsX2Pearson_discrete[ni, , iter]  <- X2Fun(Y_discrete, group_list, corMethod = "pearson")
      simResultsX2Spearman_discrete[ni, , iter] <- X2Fun(Y_discrete, group_list, corMethod = "spearman")

      # CFA
      simResultsCFA_discrete[ni, , iter] <- fitMeasures(cfa(modelCorrect, data = Y_discrete), GOFstats)

      # make sure output is not retained into next round, in case next iteration fails
      rm(out)
    })
    
    save(simResultsT1, simResultsT2, simResultsG1, simResultsG2,
         simResultsX2Pearson, simResultsX2Spearman, simResultsCFA,
         simResultsT1_discrete, simResultsT2_discrete, 
         simResultsG1_discrete, simResultsG2_discrete,
         simResultsX2Pearson_discrete, simResultsX2Spearman_discrete,
         simResultsCFA_discrete,
         file = "simCFA_1000_tG.Rdata")
  }
}
