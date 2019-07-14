library(ggplot2)
library(reshape2)
library(dplyr)
library(MASS)

if(length(grep("bdsegal",getwd()))>0 ){
  computer <- "C:/Users/bdsegal/"
} else{
  computer <- "/home/bsegal/"
}

source("simFunctions.R")
paperPath <- file.path(computer, 
  "Dropbox/Research/PermTest/MatrixBlocksTest/paper/matrix_test_paper_psychometrika")

# plot correlation matrices ---------------------------------------------------
pk <- c(5, 7, 9, 11)
pkCum <- cumsum(pk)
pkCumRev <- rev(cumsum(rev(pk)))
mu <- rep(0, sum(pk)) 

# off-diagonal corelation
a <- 0.5
sigmaCor <- array(rep(a, sum(pk)^2), dim = c(sum(pk), sum(pk)))
diag(sigmaCor) <- 1 

# randomly generated standard deviations
set.seed(1)
s <- diag(abs(rnorm(n = sum(pk), mean = 1, sd = 1)))
# True covariance matrix
sigma <- s %*% sigmaCor %*% s

# n = 10
y <- mvrnorm(n = 10, mu = mu, Sigma = sigma)
yCat <- ordFun(y)

A <- cor(yCat, method = "spearman")
diag(A) <- NA

Amelt <- melt(abs(A))
names(Amelt) <- c("x","y","value")
Amelt$y <- factor(Amelt$y, levels=rev(levels(factor(Amelt$y))))
Amelt$x <- factor(Amelt$x)

ggplot(aes(x=x, y=y, fill=abs(value)), data=Amelt)+
	geom_tile()+
	theme_bw(32)+
	scale_fill_gradient2(space="Lab", name = "", lim = c(0, 1))+
	labs(x="", y="")+
  scale_x_discrete(breaks = "")+
  scale_y_discrete(breaks = "")+
	theme(axis.text.x = element_text(angle = 90, vjust = .35,hjust=1))+
	geom_segment(x=0.5,xend=pkCum[1]+0.5,y=pkCumRev[1]+0.5,yend=pkCumRev[1]+0.5, size=1)+
	geom_segment(x=0.5,xend=pkCum[1]+0.5,y=pkCumRev[2]+0.5,yend=pkCumRev[2]+0.5, size=1)+
	geom_segment(x=0.5,xend=0.5,y=pkCumRev[2]+0.5,yend=pkCumRev[1]+0.5, size=1)+
	geom_segment(x=pkCum[1]+0.5,xend=pkCum[1]+0.5,y=pkCumRev[2]+0.5,yend=pkCumRev[1]+0.5, size=1)+
	
	geom_segment(x=pkCum[1]+0.5,xend=pkCum[2]+0.5,y=pkCumRev[2]+0.5,yend=pkCumRev[2]+0.5, size=1)+
	geom_segment(x=pkCum[1]+0.5,xend=pkCum[2]+0.5,y=pkCumRev[3]+0.5,yend=pkCumRev[3]+0.5, size=1)+
	geom_segment(x=pkCum[1]+0.5,xend=pkCum[1]+0.5,y=pkCumRev[3]+0.5,yend=pkCumRev[2]+0.5, size=1)+
	geom_segment(x=pkCum[2]+0.5,xend=pkCum[2]+0.5,y=pkCumRev[3]+0.5,yend=pkCumRev[2]+0.5, size=1)+
	
	geom_segment(x=pkCum[2]+0.5,xend=pkCum[3]+0.5,y=pkCumRev[3]+0.5,yend=pkCumRev[3]+0.5, size=1)+
	geom_segment(x=pkCum[2]+0.5,xend=pkCum[3]+0.5,y=pkCumRev[4]+0.5,yend=pkCumRev[4]+0.5, size=1)+
	geom_segment(x=pkCum[2]+0.5,xend=pkCum[2]+0.5,y=pkCumRev[4]+0.5,yend=pkCumRev[3]+0.5, size=1)+
	geom_segment(x=pkCum[3]+0.5,xend=pkCum[3]+0.5,y=pkCumRev[4]+0.5,yend=pkCumRev[3]+0.5, size=1)+
	
	geom_segment(x=pkCum[3]+0.5,xend=pkCum[4]+0.5,y=pkCumRev[4]+0.5,yend=pkCumRev[4]+0.5, size=1)+
	geom_segment(x=pkCum[3]+0.5,xend=pkCum[4]+0.5,y=0.5,yend=0.5, size=1)+
	geom_segment(x=pkCum[3]+0.5,xend=pkCum[3]+0.5,y=0.5,yend=pkCumRev[4]+0.5, size=1)+
	geom_segment(x=pkCum[4]+0.5,xend=pkCum[4]+0.5,y=0.5,yend=pkCumRev[4]+0.5, size=1)
ggsave(file.path(paperPath,"simConstant10.png"))

# n = 100
y <- mvrnorm(n = 100, mu = mu, Sigma = sigma)
yCat <- ordFun(y)

A <- cor(yCat, method = "spearman")
diag(A) <- NA

Amelt <- melt(abs(A))
names(Amelt) <- c("x","y","value")
Amelt$y <- factor(Amelt$y, levels=rev(levels(factor(Amelt$y))))
Amelt$x <- factor(Amelt$x)

ggplot(aes(x=x, y=y, fill=abs(value)), data=Amelt)+
	geom_tile()+
	theme_bw(32)+
	scale_fill_gradient2(space="Lab", name = "", lim = c(0, 1))+
  labs(x="", y="")+
  scale_x_discrete(breaks = "")+
  scale_y_discrete(breaks = "")+
	theme(axis.text.x = element_text(angle = 90, vjust = .35,hjust=1))+
	geom_segment(x=0.5,xend=pkCum[1]+0.5,y=pkCumRev[1]+0.5,yend=pkCumRev[1]+0.5, size=1)+
	geom_segment(x=0.5,xend=pkCum[1]+0.5,y=pkCumRev[2]+0.5,yend=pkCumRev[2]+0.5, size=1)+
	geom_segment(x=0.5,xend=0.5,y=pkCumRev[2]+0.5,yend=pkCumRev[1]+0.5, size=1)+
	geom_segment(x=pkCum[1]+0.5,xend=pkCum[1]+0.5,y=pkCumRev[2]+0.5,yend=pkCumRev[1]+0.5, size=1)+
	
	geom_segment(x=pkCum[1]+0.5,xend=pkCum[2]+0.5,y=pkCumRev[2]+0.5,yend=pkCumRev[2]+0.5, size=1)+
	geom_segment(x=pkCum[1]+0.5,xend=pkCum[2]+0.5,y=pkCumRev[3]+0.5,yend=pkCumRev[3]+0.5, size=1)+
	geom_segment(x=pkCum[1]+0.5,xend=pkCum[1]+0.5,y=pkCumRev[3]+0.5,yend=pkCumRev[2]+0.5, size=1)+
	geom_segment(x=pkCum[2]+0.5,xend=pkCum[2]+0.5,y=pkCumRev[3]+0.5,yend=pkCumRev[2]+0.5, size=1)+
	
	geom_segment(x=pkCum[2]+0.5,xend=pkCum[3]+0.5,y=pkCumRev[3]+0.5,yend=pkCumRev[3]+0.5, size=1)+
	geom_segment(x=pkCum[2]+0.5,xend=pkCum[3]+0.5,y=pkCumRev[4]+0.5,yend=pkCumRev[4]+0.5, size=1)+
	geom_segment(x=pkCum[2]+0.5,xend=pkCum[2]+0.5,y=pkCumRev[4]+0.5,yend=pkCumRev[3]+0.5, size=1)+
	geom_segment(x=pkCum[3]+0.5,xend=pkCum[3]+0.5,y=pkCumRev[4]+0.5,yend=pkCumRev[3]+0.5, size=1)+
	
	geom_segment(x=pkCum[3]+0.5,xend=pkCum[4]+0.5,y=pkCumRev[4]+0.5,yend=pkCumRev[4]+0.5, size=1)+
	geom_segment(x=pkCum[3]+0.5,xend=pkCum[4]+0.5,y=0.5,yend=0.5, size=1)+
	geom_segment(x=pkCum[3]+0.5,xend=pkCum[3]+0.5,y=0.5,yend=pkCumRev[4]+0.5, size=1)+
	geom_segment(x=pkCum[4]+0.5,xend=pkCum[4]+0.5,y=0.5,yend=pkCumRev[4]+0.5, size=1)
ggsave(file.path(paperPath,"simConstant100.png"))

# n = 1000
y <- mvrnorm(n = 1000, mu = mu, Sigma = sigma)
yCat <- ordFun(y)

A <- cor(yCat, method = "spearman")
diag(A) <- NA

Amelt <- melt(abs(A))
names(Amelt) <- c("x","y","value")
Amelt$y <- factor(Amelt$y, levels=rev(levels(factor(Amelt$y))))
Amelt$x <- factor(Amelt$x)

ggplot(aes(x=x, y=y, fill=abs(value)), data=Amelt)+
	geom_tile()+
	theme_bw(32)+
	scale_fill_gradient2(space="Lab", name = "", lim = c(0, 1))+
  labs(x="", y="")+
  scale_x_discrete(breaks = "")+
  scale_y_discrete(breaks = "")+
	theme(axis.text.x = element_text(angle = 90, vjust = .35,hjust=1))+
	geom_segment(x=0.5,xend=pkCum[1]+0.5,y=pkCumRev[1]+0.5,yend=pkCumRev[1]+0.5, size=1)+
	geom_segment(x=0.5,xend=pkCum[1]+0.5,y=pkCumRev[2]+0.5,yend=pkCumRev[2]+0.5, size=1)+
	geom_segment(x=0.5,xend=0.5,y=pkCumRev[2]+0.5,yend=pkCumRev[1]+0.5, size=1)+
	geom_segment(x=pkCum[1]+0.5,xend=pkCum[1]+0.5,y=pkCumRev[2]+0.5,yend=pkCumRev[1]+0.5, size=1)+
	
	geom_segment(x=pkCum[1]+0.5,xend=pkCum[2]+0.5,y=pkCumRev[2]+0.5,yend=pkCumRev[2]+0.5, size=1)+
	geom_segment(x=pkCum[1]+0.5,xend=pkCum[2]+0.5,y=pkCumRev[3]+0.5,yend=pkCumRev[3]+0.5, size=1)+
	geom_segment(x=pkCum[1]+0.5,xend=pkCum[1]+0.5,y=pkCumRev[3]+0.5,yend=pkCumRev[2]+0.5, size=1)+
	geom_segment(x=pkCum[2]+0.5,xend=pkCum[2]+0.5,y=pkCumRev[3]+0.5,yend=pkCumRev[2]+0.5, size=1)+
	
	geom_segment(x=pkCum[2]+0.5,xend=pkCum[3]+0.5,y=pkCumRev[3]+0.5,yend=pkCumRev[3]+0.5, size=1)+
	geom_segment(x=pkCum[2]+0.5,xend=pkCum[3]+0.5,y=pkCumRev[4]+0.5,yend=pkCumRev[4]+0.5, size=1)+
	geom_segment(x=pkCum[2]+0.5,xend=pkCum[2]+0.5,y=pkCumRev[4]+0.5,yend=pkCumRev[3]+0.5, size=1)+
	geom_segment(x=pkCum[3]+0.5,xend=pkCum[3]+0.5,y=pkCumRev[4]+0.5,yend=pkCumRev[3]+0.5, size=1)+
	
	geom_segment(x=pkCum[3]+0.5,xend=pkCum[4]+0.5,y=pkCumRev[4]+0.5,yend=pkCumRev[4]+0.5, size=1)+
	geom_segment(x=pkCum[3]+0.5,xend=pkCum[4]+0.5,y=0.5,yend=0.5, size=1)+
	geom_segment(x=pkCum[3]+0.5,xend=pkCum[3]+0.5,y=0.5,yend=pkCumRev[4]+0.5, size=1)+
	geom_segment(x=pkCum[4]+0.5,xend=pkCum[4]+0.5,y=0.5,yend=pkCumRev[4]+0.5, size=1)
ggsave(file.path(paperPath,"simConstant1000.png"))

# plots simulation results ----------------------------------------------------

load("simConstant_1000_tG.Rdata")

simResults <- simResultsG2

simResultsM <- melt(simResults)
simResultsM$n <- factor(simResultsM$n, labels = paste("n = ", c(10, 100, "1,000"),sep = ""))
simResultsM$k <- factor(simResultsM$p, labels = c("Overall", paste("k = ", 1:4, sep="")))

# false alarm rate with gamma_norm: table
alpha <- c(0.01, 0.05)
falseAlarm <- array(dim = c(3, 2, 2),
  dimnames = list(n = c(10, 100, 1000),
    block = c("Overall", "Block-specific FWER"),
    alpha = alpha
  )
)

for (n in 1:3){
  for (a in 1:2){
    for (k in 1:2) {
      falseAlarm[n, k, a] <- mean(simResults[n, k, ] <= alpha[a], na.rm = TRUE)  
      if (k == 2) {
        falseAlarm[n, k, a] <- 
        mean(apply(simResults[n, k:5, ], 2, min)   <= alpha[a], na.rm = TRUE)
      }
    }
  }
}
signif(falseAlarm, 2)

# RMSEA: plot and table
simResultsCFArmsea <- melt(simResultsCFA[, "rmsea", ])
simResultsCFArmsea$n <- factor(simResultsCFArmsea$n, labels = paste("n = ", c(10, 100, "1,000"), sep = ""))
simResultsCFArmsea$p <- "RMSEA"

dev.new(height = 3, width = 8)
ggplot(aes(x = value), data = simResultsCFArmsea)+
  geom_histogram()+
  facet_grid(~ n, scales = "free")+
  theme_bw(18) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Count", x = "")
ggsave(file.path(paperPath, "simConstant_rmsea.png"))

group_by(simResultsCFArmsea, n) %>%
  summarize(
    alpha05 = mean(value < 0.05, na.rm = TRUE),
    alpha07 = mean(value < 0.07, na.rm = TRUE),
    alpha1 = mean(value < 0.1, na.rm = TRUE))

# CFA: CFI and TLI: table
simResultsCFAM <- melt(simResultsCFA[, c("cfi", "tli"), ])
simResultsCFAM$n <- factor(simResultsCFAM$n, labels = paste("n = ", c(10, 100, "1,000"),sep = ""))
simResultsCFAM$stat <- factor(simResultsCFAM$p, labels = c("CFI", "TLI"))

group_by(simResultsCFAM, stat, n) %>%
  summarize(
    alpha95 = mean(value >= 0.95, na.rm = TRUE),
    alpha90 = mean(value >= 0.9, na.rm = TRUE),
    alpha80 = mean(value >= 0.8, na.rm = TRUE))

# X2 with Pearson correlation
simResultsX2PearsonM <- melt(simResultsX2Pearson[, "pval", ])
simResultsX2PearsonM$n <- factor(simResultsX2PearsonM$n, labels = paste("n = ", c(10, 100, "1,000"), sep = ""))

# X2 with Spearman's correlation
simResultsX2SpearmanM <- melt(simResultsX2Spearman[, "pval", ])
simResultsX2SpearmanM$n <- factor(simResultsX2SpearmanM$n, labels = paste("n = ", c(10, 100, "1,000"), sep = ""))

# plot all together
simResultsX2PearsonM$stat <- "X[2]-pval"
simResultsM$stat <- "Gamma[norm]-pval"

simAll <- simResultsM[which(simResultsM$k == "Overall"),
                      which(colnames(simResultsM) %in% c("n", "iter", "value", "stat"))]
simAll <- rbind(simAll, simResultsX2PearsonM)
simAll <- rbind(simAll, simResultsCFAM[, which(colnames(simResultsCFAM) != "p")])
simAll$stat <- factor(simAll$stat, levels = c("Gamma[norm]-pval", "X[2]-pval", "CFI", "TLI"))

dev.new(width = 8, height = 5.5)
ggplot(aes(x = value), data = simAll[which(simAll$stat != "TLI"), ])+
  geom_histogram(binwidth = 0.075)+
  facet_grid(stat ~ n, labeller = labeller(.rows = label_parsed), scale = "free_y")+
  theme_bw(18)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = "Count")
ggsave(file.path(paperPath, "simConst_hist_all.png"))
