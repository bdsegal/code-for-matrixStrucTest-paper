
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
paperPath <- file.path(computer, "Dropbox/Research/PermTest/MatrixBlocksTest/paper/constrValid")
paperPlotPath <- file.path(computer,
   "Dropbox/Research/PermTest/MatrixBlocksTest/paper/matrix_test_paper/plots")

load("simWhiteNoise_1000_tG.Rdata")

# plot correlation matrices ---------------------------------------------------
pk <- c(5, 7, 9, 11)
pkCum <- cumsum(pk)
pkCumRev <- rev(cumsum(rev(pk)))
mu <- rep(0, sum(pk)) 

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
	theme_bw(30)+
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
ggsave(file.path(paperPlotPath, "simWhiteNoise10.png"))

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
	theme_bw(30)+
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
ggsave(file.path(paperPlotPath, "simWhiteNoise100.png"))

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
	theme_bw(30)+
  scale_fill_gradient2(space="Lab", name = "", lim = c(0, 1))+
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
ggsave(file.path(paperPlotPath, "simWhiteNoise1000.png"))


# plots simulation results ----------------------------------------------------

load("simWhiteNoise_1000_tG.Rdata")

# simResults <- simResultsT1
# simResults <- simResultsT2
# simResults <- simResultsG1
simResults <- simResultsG2

simResultsM <- melt(simResults)
simResultsM$n <- factor(simResultsM$n, labels = paste("n = ", c(10, 100, "1,000"),sep = ""))
simResultsM$k <- factor(simResultsM$p, labels = c("Overall", paste("k = ", 1:4, sep="")))

ggplot(aes(x = value), data = simResultsM)+
  geom_histogram(binwidth = .05)+
  facet_grid(k ~ n)+
  theme_bw(18)+
  labs(x = "p-value")+
  scale_x_continuous(lim = c(0, 1), labels = c(0, 0.25, 0.5, 0.75, 1), breaks = c(0, 0.25, 0.5, 0.75, 1))
  # geom_vline(xintercept = log(c(0.01, 0.05)), color = "red", linetype = "dashed")
ggsave(file.path(paperPath, "simWhiteNoiseHist.png"))

# false alarm rate
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
# , , alpha = 0.01

#       block
# n      Overall Block-specific FWER
#   10     0.012               0.011
#   100    0.010               0.008
#   1000   0.012               0.011

# , , alpha = 0.05

#       block
# n      Overall Block-specific FWER
#   10     0.054               0.055
#   100    0.044               0.048
#   1000   0.058               0.054

# X2 with Pearson correlation
simResultsX2PearsonM <- melt(simResultsX2Pearson[, "pval", ])
simResultsX2PearsonM$n <- factor(simResultsX2PearsonM$n, labels = paste("n = ", c(10, 100, "1,000"),sep = ""))
ggplot(aes(x = value), data = simResultsX2PearsonM)+
  geom_histogram()+
  facet_grid( ~ n)+
  theme_bw(18)+
  labs(x = "p-value")
  scale_x_continuous(lim = c(0, 1), labels = c(0, 0.25, 0.5, 0.75, 1), breaks = c(0, 0.25, 0.5, 0.75, 1))
  # geom_vline(xintercept = log(c(0.01, 0.05)), color = "red", linetype = "dashed")

# X2 with Spearman's correlation
simResultsX2SpearmanM <- melt(simResultsX2Spearman[, "pval", ])
simResultsX2SpearmanM$n <- factor(simResultsX2SpearmanM$n, labels = paste("n = ", c(10, 100, "1,000"),sep = ""))
ggplot(aes(x = value), data = simResultsX2SpearmanM)+
  geom_histogram()+
  facet_grid( ~ n)+
  theme_bw(18)+
  labs(x = "p-value")+
  scale_x_continuous(labels = c(0, 0.25, 0.5, 0.75, 1), breaks = c(0, 0.25, 0.5, 0.75, 1))
  # geom_vline(xintercept = log(c(0.01, 0.05)), color = "red", linetype = "dashed")

# CFA: CFI and TLI
simResultsCFAM <- melt(simResultsCFA[, c("cfi", "tli"), ])
simResultsCFAM$n <- factor(simResultsCFAM$n, labels = paste("n = ", c(10, 100, "1,000"),sep = ""))
simResultsCFAM$p <- factor(simResultsCFAM$p, labels = c("CFI", "TLI"))
dev.new(height = 5, width = 8)
ggplot(aes(x = value), data = simResultsCFAM)+
  geom_histogram()+
  facet_grid(p~ n)+
  theme_bw(18)
ggsave(file.path(paperPlotPath, "simWhite_cfi_tfi.png"))

group_by(simResultsCFAM, p, n) %>%
  summarize(
    alpha95 = mean(value >= 0.95, na.rm = TRUE),
    alpha90 = mean(value >= 0.9, na.rm = TRUE),
    alpha80 = mean(value >= 0.8, na.rm = TRUE))
#        p         n  alpha95  alpha90   alpha80
#   (fctr)    (fctr)    (dbl)    (dbl)     (dbl)
# 1    CFI    n = 10 0.869186 0.877907 0.9069767
# 2    CFI   n = 100 0.000000 0.000000 0.0000000
# 3    CFI n = 1,000 0.000000 0.000000 0.0000000
# 4    TLI    n = 10 0.869186 0.877907 0.8982558
# 5    TLI   n = 100 0.000000 0.000000 0.0000000
# 6    TLI n = 1,000 0.000000 0.000000 0.0000000


# plot all together
simResultsX2PearsonM$stat <- "X[2]-pval"
simResultsM$stat <- "Gamma[norm]-pval"
simResultsCFAM$stat <- simResultsCFAM$p

simAll <- simResultsM[which(simResultsM$k == "Overall"),
                      which(colnames(simResultsM) %in% c("n", "iter", "value", "stat"))]
simAll <- rbind(simAll, simResultsX2PearsonM)
simAll <- rbind(simAll, simResultsCFAM[, which(colnames(simResultsCFAM) != "p")])
simAll$stat <- factor(simAll$stat, levels = c("Gamma[norm]-pval", "X[2]-pval", "CFI", "TLI"))

ggplot(aes(x = value), data = simAll[which(simAll$stat != "TLI"), ])+
  geom_histogram(binwidth = .075)+
  facet_grid(stat ~ n, labeller = labeller(.rows = label_parsed), scale = "free_y")+
  theme_bw(18)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(paperPlotPath, "simWhite_hist_all.png"))
