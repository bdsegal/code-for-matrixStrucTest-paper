# Plots of HRS data and related visuals

library(data.table)
library(ggplot2)
library(reshape2)
library(matrixTest)

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

# get A matrix ----------------------------------------------------------------
A <- abs(cor(subBig5, use = "complete.obs", method = "spearman"))
diag(A) <- NA

# define blocks
extro <- which(colnames(A) %in% c("a","f","j","u","z2"))
agree <- which(colnames(A) %in% c("b","g","k","p","y"))
cons <- which(colnames(A) %in% c("c","e","i","n","r", "v", "x", "z", "z5", "z6"))
neuro <- which(colnames(A) %in% c("d", "h", "l","q"))
open <- which(colnames(A) %in% c("m","o","s","t","w","z3","z4"))

# quantities for plotting ---------------------------------
ord <- c(neuro, extro, agree, open, cons)
pk <- sapply(list(neuro, extro, agree, open, cons), length)
pkCum <- cumsum(pk)
pkCumRev <- rev(cumsum(rev(pk)))

Am <- melt(A)
names(Am) <- c("x", "y", "value")
Am$y <- factor(Am$y, levels=rev(levels(Am$y)))

ggplot(aes(x=x, y=y, fill=value),data=Am)+
  geom_tile()+
  theme_bw(18)+
  scale_fill_gradient2(space="Lab", name="abs(Cor)", lim = c(0, 1))+
  labs(x="", y="")+
  theme(axis.text.x = element_text(angle = 90, vjust = .35,hjust=1))
# ggsave(paste(paper_plot_path,"HRS_big5_heatmap_origOrder.png",sep=""))
  
Am <- melt(A[ord, ord])
names(Am) <- c("x","y","value")
Am$y <- factor(Am$y, levels=rev(levels(Am$y)))

ggplot(aes(x=x, y=y, fill=abs(value)),data=Am)+
  geom_tile()+
  theme_bw(18)+
  scale_fill_gradient2(space="Lab", name="abs(Cor)", lim = c(0, 1))+
  labs(x="", y="")+
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
  geom_segment(x=pkCum[3]+0.5,xend=pkCum[4]+0.5,y=pkCumRev[5]+0.5,yend=pkCumRev[5]+0.5, size=1)+
  geom_segment(x=pkCum[3]+0.5,xend=pkCum[3]+0.5,y=pkCumRev[5]+0.5,yend=pkCumRev[4]+0.5, size=1)+
  geom_segment(x=pkCum[4]+0.5,xend=pkCum[4]+0.5,y=pkCumRev[5]+0.5,yend=pkCumRev[4]+0.5, size=1)+
  
  geom_segment(x=pkCum[4]+0.5,xend=pkCum[5]+0.5,y=pkCumRev[5]+0.5,yend=pkCumRev[5]+0.5, size=1)+
  geom_segment(x=pkCum[4]+0.5,xend=pkCum[5]+0.5,y=0.5,yend=0.5, size=1)+
  geom_segment(x=pkCum[4]+0.5,xend=pkCum[4]+0.5,y=0.5,yend=pkCumRev[5]+0.5, size=1)+
  geom_segment(x=pkCum[5]+0.5,xend=pkCum[5]+0.5,y=0.5,yend=pkCumRev[5]+0.5, size=1)
ggsave(file.path(paperPlotPath,"HRS_big5_heatmap.png"))

# box plots -- not in paper
group_list <- list(neuro, extro, agree, open, cons)

box <- prepBoxPlots(A = A, group_list, absolute = TRUE)

ggplot(aes(x=as.factor(delta), y=a), data=box$overall)+
  geom_boxplot()+
  theme_bw(28)+
  labs(x=expression(Delta), y="|a|")
ggsave(file.path(paperPlotPath,"HRS_big5_box_overall.png"))

dev.new(width=12,height=5)
ggplot(aes(x=as.factor(delta), y=a), data=box$multi)+
  geom_boxplot()+
  facet_grid(~block)+
  theme_bw(28)+
labs(x=expression(Delta), y="|a|")
ggsave(file.path(paperPlotPath,"HRS_big5_box_multi.png"))
