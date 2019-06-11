# Load libraries
library(igraph)
library(dplyr)
library(stringr)
library(Matrix)
library(reshape2)

#Create Results directory
dir.create("./res", showWarnings = FALSE, recursive = T)

# set global options
options(stringsAsFactors = FALSE)

# d1 <- read.table("3.txt", sep = " ")
# d1 <- readLines("3.txt")

setwd("861条评论/")
filename <- dir(path = "./")
filename <- paste0(1:861, ".txt")
d <- sapply(filename, function(x){
  tmp <- readLines(x)
  paste(tmp, collapse = "")
  
})
setwd("../")

term <- c("行花街","买年花","赏花","品花香","解花语","办年货","转运","行大运","祈福", 
"每年必逛","一年一度","情怀","花街","花市牌楼","花市棚架","西关风情","鲜花","风车", 
"工艺品","饰品","年桔","桃花","热闹","传统","习俗","年味浓","节日气氛","特色","喜庆",
"漂亮","拥挤","历史悠久","花的海洋","人山人海","西湖路","教育路","北京路","天河体育中心", 
"滨江路","荔湾路","岭南文化","广府文化","十二生肖","灯笼","挥春","越秀西湖花市", 
"天河花市","海珠花市","荔湾花市","中心花市")

tdm <- Matrix(0, nrow = 861, ncol = 50)
colnames(tdm) <- term
for(i in 1:nrow(tdm)){
  tmp <- str_extract_all(d[i], term)
  tdm[i, ] <- sapply(tmp, length)
}

write.table(as.matrix(tdm), "res/tdm.txt", col.names = NA, quote = F, sep = "\t")

tdm2 <- tdm 
tdm2[tdm >1] <- 1
com <- t(tdm2) %*% tdm2
write.table(as.matrix(com), "res/com.txt", col.names = NA, quote = F, sep = "\t")

resd <- data.frame(term=term, freq=diag(com))
write.table(resd, "res/freq.txt", row.names = F, quote = F, sep = "\t")

term <- data.frame(term, type=rep(1:8, times=c(6,6,3,7,12,6,5,5)))

gephi <- Matrix::summary(tdm)
length(unique(gephi$i))#843条有效评论

term$id <- 1:50
gephi$j <- term$type[match(gephi$j, term$id)]
write.csv(gephi, "res/gephi.csv", quote = F, row.names = F)

tdm8 <- dcast(gephi, i~j, value.var = "x")
dim(tdm8)
rownames(tdm8) <- tdm8[,1]
tdm8 <- tdm8[,-1]
TF <- data.frame(class_id=1:8, class_name=c("身体实践","仪式","场要素","景要素","综合感知要素","地名","文化元素","地标"))
tdm9 <- tdm8
tdm9[tdm9>1] <- 1
TF$tf <- colSums(tdm9)/sum(colSums(tdm9))
TF$tf2 <- colSums(tdm9)

write.table(TF, "res/TF.txt", row.names = F, quote = F, sep = "\t")

g <- graph_from_incidence_matrix(tdm8, weighted = T)
g# 851 2973

write.graph(g, "res/tdm8.gml", "gml")

class <- readxl::read_excel("res/节点属性.xlsx")
table(class$modularity_class)
