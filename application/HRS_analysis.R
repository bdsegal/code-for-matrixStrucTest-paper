# Analysis of HRS data using 1) Hubert's Gamma (permutation test), 2) CFA, 
# and 3) Steiger's method

library(data.table)
library(ggplot2)
library(pheatmap)
library(reshape2)
library(lavaan)
library(matrixStrucTest)

# setup paths
if(length(grep("bdsegal",getwd()))>0 ){
  computer <- "C:/Users/bdsegal"
} else{
	computer <- "/home/bsegal"
}

dataPath <- file.path(computer,
	"Dropbox/Research/PermTest/MatrixBlocksTest/data_examples/HRS/lb_04_10")
# paper_plot_path <- paste(computer,"Dropbox/Research/PermTest/MatrixBlocksTest/paper/constrValid/",sep="")
# paperPath <- file.path(computer,
# 	 "Dropbox/Research/PermTest/MatrixBlocksTest/paper/matrix_test_paper/plots")
paperPath <- file.path(computer, 
  "Dropbox/Research/PermTest/MatrixBlocksTest/paper/matrix_test_paper_psychometrika")

# read in and process the HRS leave behind (lb) 2010 data ---------------------
H10 <- fread(file.path(dataPath, "H10.csv"), data.table = FALSE)

# keep columns corresponding to survey questions, which begin with "Q"
keep <- grep("^Q", colnames(H10))
sub <- H10[, keep]

# get which questionnaire each question/item belongs to
pos <- regexpr("[0-9]{2}", colnames(sub))
questionnaire <- as.factor(regmatches(colnames(sub), pos))

# get subset of items for the big five questionnaire
subBig5 <- sub[, which(questionnaire == 33)]
# replace long column names with short item names
end <- regexpr("\\.", colnames(subBig5))
itemNames <- tolower(mapply(function(nam, t) {substr(nam, 4, t - 1)}, 
                            colnames(subBig5), end))
colnames(subBig5) <- gsub("_", "", itemNames)

# reverse code items ----------------------------------------------------------
# See https://hrs.isr.umich.edu/sites/default/files/biblio/HRS2006-2010SAQdoc.pdf
revCode <- function(x) {
  (x==4)*1 + (x==3)*2 + (x==2)*3 + (x==1)*4
}

subBig5$c <- revCode(subBig5$c)
subBig5$q <- revCode(subBig5$q)
subBig5$v <- revCode(subBig5$v)
subBig5$x <- revCode(subBig5$x)

# missingness -----------------------------------------------------------------

barplot(apply(subBig5, 2, function(x){mean(is.na(x))}), main = "percent missing")

subBig5noNA <- na.omit(subBig5)
nrow(sub)

nrow(subBig5noNA)

numNA <- apply(subBig5,1,function(x){sum(is.na(x))})
length(numNA[which(numNA < ncol(subBig5))]) - length(numNA[which(numNA == 0)])
	
nrow(subBig5noNA) / nrow(sub)
	
# get A matrix ----------------------------------------------------------------
A <- abs(cor(subBig5, use = "complete.obs", method = "spearman"))

# define blocks
neuro <- which(colnames(A) %in% c("d", "h", "l","q"))
extro <- which(colnames(A) %in% c("a","f","j","u","z2"))
agree <- which(colnames(A) %in% c("b","g","k","p","y"))
open <- which(colnames(A) %in% c("m","o","s","t","w","z3","z4"))
cons <- which(colnames(A) %in% c("c","e","i","n","r", "v", "x", "z", "z5", "z6"))

group_list <- list(neuro = neuro, extro = extro, agree = agree, open = open, cons = cons)

out <- matrixStrucTest(A = A, group_list = group_list, B = 10000, absolute = TRUE)
out

dev.new(width = 6, height = 4)
qplot(x=out$Gamma_overall, geom="histogram")+
	theme_bw(22)+
	geom_vline(xintercept=out$Gamma0, color="red")+
	labs(x=expression(Gamma["norm"]))
ggsave(file.path(paperPath,"HRS_big5_overall.png"))

dev.new(width = 6, height = 4)
qplot(x=out$Gamma_max_one_sided, geom="histogram", binwidth=.015)+
	theme_bw(22)+
	geom_vline(xintercept=out$Gamma0k, color="red")+
	annotate("text", x = out$Gamma0k +.03, y = c(610, rep(800,2),700,800), label = paste("k=",names(out$Gamma0k),sep=""),size=7)+
	labs(x=expression(Gamma["norm"]^"max"))
ggsave(file.path(paperPath,"HRS_big5_multi.png"))

# with CFA --------------------------------------------------------------------
for (i in 1:ncol(subBig5)){
	subBig5[,i] <- as.ordered(subBig5[,i])
}

model <- 'extro =~ a + f + j + u + z2
agree =~ b + g + k + p + y
cons =~ c + e + i + n + r + v + x + z + z5 + z6
neuro =~ d + h + l + q
open =~ m + o + s + t + w + z3 + z4'

GOFstats <- c("chisq", "df", "pvalue", 
              "chisq.scaled", "df.scaled", "pvalue.scaled",
              "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper")

cfaFit <- cfa(model, data=subBig5) #, sample.cov=TRUE)
summary(cfaFit, fit.measures=TRUE)
fitMeasures(cfaFit, GOFstats)


# with Steiger's method -------------------------------------------------------
subBig5 <- sub[, which(questionnaire == 33)]

X2out <- X2Fun(subBig5, group_list, corMethod = "spearman")
X2out

X2out <- X2Fun(subBig5, group_list, corMethod = "pearson")
X2out
