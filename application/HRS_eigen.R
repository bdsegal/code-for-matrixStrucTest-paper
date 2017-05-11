# demonstration of horizontal/vertical structure in low rank approximations

library(data.table)
library(ggplot2)
library(reshape2)

# setup paths
if(length(grep("bdsegal",getwd()))>0 ){
  computer <- "C:/Users/bdsegal"
} else{
  computer <- "/home/bsegal"
}

dataPath <- file.path(computer,
  "Dropbox/Research/PermTest/MatrixBlocksTest/data_examples/HRS/lb_04_10")
# paper_plot_path <- paste(computer,"Dropbox/Research/PermTest/MatrixBlocksTest/paper/constrValid/",sep="")
paperPlotPath <- file.path(computer,
   "Dropbox/Research/PermTest/MatrixBlocksTest/paper/matrix_test_paper/plots")

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

# demonstration of block diagonal structure implied by top eigenvectors
A <- abs(cor(subBig5, use = "complete.obs", method = "spearman"))

extro <- which(colnames(A) %in% c("a","f","j","u","z2"))
agree <- which(colnames(A) %in% c("b","g","k","p","y"))
cons <- which(colnames(A) %in% c("c","e","i","n","r", "v", "x", "z", "z5", "z6"))
neuro <- which(colnames(A) %in% c("d", "h", "l","q"))
open <- which(colnames(A) %in% c("m","o","s","t","w","z3","z4"))
ord <- c(neuro, extro, agree, open, cons)

eig <- eigen(abs(A))
AList <- with(eig, lapply(1:3, function(i) values[i] * outer(vectors[, i], vectors[, i])))
A1 <- AList[[1]]
dimnames(A1) <- dimnames(A)

A1_m <- melt(A1)
colnames(A1_m) <- c("x","y","value")
A1_m$y <- factor(A1_m$y, levels=rev(levels(A1_m$y)))
ggplot(aes(x=x, y=y, fill=value),data=A1_m)+
  geom_tile()+
  theme_bw(20)+
  scale_fill_gradient2(space="Lab", name = "abs(Cor)")+
  labs(x="", y="")+
  theme(axis.text.x = element_text(angle = 90, vjust = .35,hjust=1))
ggsave(file.path(paperPlotPath, "HRS_eigen1.png"))
  
A1_m <- melt(A1[ord, ord])
colnames(A1_m) <- c("x","y","value")
A1_m$y <- factor(A1_m$y, levels=rev(levels(A1_m$y)))
ggplot(aes(x=x, y=y, fill=value),data=A1_m)+
  geom_tile()+
  theme_bw(20)+
  scale_fill_gradient2(space="Lab", name = "abs(Cor)")+
  labs(x="", y="")+
  theme(axis.text.x = element_text(angle = 90, vjust = .35,hjust=1))
ggsave(file.path(paperPlotPath, "HRS_eigen1_ord.png"))

# with two eigenvalues
A1 <- AList[[1]] + AList[[2]]
dimnames(A1) <- dimnames(A)

A1_m <- melt(A1)
colnames(A1_m) <- c("x","y","value")
A1_m$y <- factor(A1_m$y, levels=rev(levels(A1_m$y)))
ggplot(aes(x=x, y=y, fill=value),data=A1_m)+
  geom_tile()+
  theme_bw(18)+
  scale_fill_gradient2(space="Lab", name = "abs(Cor)")+
  labs(x="", y="")+
  theme(axis.text.x = element_text(angle = 90, vjust = .35,hjust=1))
ggsave(file.path(paperPlotPath, "HRS_eigen2.png"))
  
A1_m <- melt(A1[ord, ord])
colnames(A1_m) <- c("x","y","value")
A1_m$y <- factor(A1_m$y, levels=rev(levels(A1_m$y)))
ggplot(aes(x=x, y=y, fill=value),data=A1_m)+
  geom_tile()+
  theme_bw(18)+
  scale_fill_gradient2(space="Lab", name = "abs(Cor)")+
  labs(x="", y="")+
  theme(axis.text.x = element_text(angle = 90, vjust = .35,hjust=1))
ggsave(file.path(paperPlotPath, "HRS_eigen2_ord.png"))