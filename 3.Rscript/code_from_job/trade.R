# Load libraries
library(igraph)
library(dplyr)
library(stringr)

#Create Results directory
dir.create("./res", showWarnings = FALSE, recursive = T)

# set global options
options(stringsAsFactors = FALSE)

# td <- data.frame(x1=rep(1:5, each=5), x2=rep(1:5, times=5), x3=sample(1:500, 25))
# td <- td[-c(1,7,13,19,25),]
# td <- td[sample(1:20, 20),]
# td <- arrange(td, x1,x2)
# matrix <- diag()
cname <- read.delim("2017.txt", header = F)
td <- data.frame(x1=rep(cname$V1, each=20), x2=rep(cname$V1, times=20))
td2 <- filter(td, x1 != x2)
write.table(td2, "countryname.txt", row.names = F, col.names = F, quote = F, sep = "\t")

dd <- as.data.frame(readxl::read_xlsx("data.xlsx"))
summary(dd[,3:16])

net2 <- function(td){
  colnames(td) <- c("c1", "c2", "money")
  # td$money[td$money < median(td$money)] <- 0
  td$money[td$money < mean(td$money)] <- 0
  # as.tbl(as.data.frame(table(dd$`2017`)))%>% 
  #   arrange(Var1)%>% 
  #   dplyr::mutate(cmfreq = cumsum(Freq)/sum(Freq)*100)%>% 
  #   plotly:: plot_ly(x=~Var1, y=~cmfreq, type = 'scatter', mode = 'lines+markers')
  td <- arrange(td, c1, c2)
  m <- matrix(0, nrow=length(unique(td[,1])), ncol=length(unique(td[,1])), 
              dimnames=list(unique(td[,1]), unique(td[,1])))
  for(i in 1:nrow(m)){
    m[i,-i] <- filter(td, c1==rownames(m)[i])$money
  }
  
  g <- graph_from_adjacency_matrix(m, mode = "directed", weighted = T)
  return(g)
}

# g1 <- net2(dd[,1:3])
# g1
# m1 <- get.adjacency(g1, attr="weight")
# write.table(as.matrix(m1), "m1.txt", sep = "\t", quote = F, col.names = NA)
# plot(g1, vertex.size=8, layout=layout.fruchterman.reingold)
# g2 <- net2(td2)
# g3 <- net2(td3)
# g4 <- net2(td4)
# g5 <- net2(td5)
# g6 <- net2(td6)
# g7 <- net2(td7)
# g8 <- net2(td8)
# g9 <- net2(td9)
# g10 <- net2(td10)
# g11 <- net2(td11)
# g12 <- net2(td12)
# g13 <- net2(td13)
# g14 <- net2(td14)

resnet <- lapply(3:16, function(x){
  net2(dd[,c(1,2,x)])
})

resnet[[2]]
# Deg.out <- sapply(resnet, function(x){
#   t1 <- degree(x, mode = "out")
#   t2 <- degree(x, mode = "in")
#   t3 <- degree(x, mode = "all")
#   cbind(t1,t2,t3)
#   })
Deg.in <- sapply(resnet, function(x){degree(x, mode = "in")/19})
colnames(Deg.in) <- paste0(colnames(dd)[3:16], "入度")
Deg.out <- sapply(resnet, function(x){degree(x, mode = "out")/19})
colnames(Deg.out) <- paste0(colnames(dd)[3:16], "出度")
Deg.all <- sapply(resnet, function(x){degree(x)/38})
colnames(Deg.all) <- paste0(colnames(dd)[3:16], "总度数")

Deg <- cbind(Deg.in, Deg.out, Deg.all)
Deg <- Deg[unique(dd$c1),]
write.table(round(Deg,4), "./res/Deg.txt", sep = "\t", quote = F, col.names = NA)
# select(as.data.frame(Deg), contains("2017"),)

Den <- as.data.frame(sapply(resnet, graph.density))
rownames(Den) <- paste0(2017:2004, "年")
write.table(Den, "./res/Den.txt", sep = "\t", quote = F, col.names = NA)

# bet.all <- lapply(c(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14), betweenness)
# clo.out <- lapply(c(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14), function(x){closeness(x, mode = "out")})
# clo.in <- lapply(c(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14), function(x){closeness(x, mode = "in")})
# Den <- lapply(c(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14), graph.density)

# write.table(Deg.out, "./res/Deg.out.txt", sep = "\t", quote = F, col.names = NA)
# write.table(Deg.in, "./res/Deg.in.txt", sep = "\t", quote = F, col.names = NA)
# write.table(bet.all, "./res/bet.all.txt", sep = "\t", quote = F, col.names = NA)
# write.table(clo.out, "./res/clo.out.txt", sep = "\t", quote = F, col.names = NA)
# write.table(clo.in, "./res/clo.in.txt", sep = "\t", quote = F, col.names = NA)
# write.table(Den, "./res/Den.txt", sep = "\t", quote = F, col.names = NA)

# strength
# ress <- sapply(unique(dd$c1), function(x){
#   colSums(as.data.frame(filter(dd, c1==x) %>% 
#                           select(contains("20"))))
# })
# 
# ress <- t(ress)

Str.in <- sapply(resnet, function(x){strength(x, mode = "in")})
colnames(Str.in) <- paste0(colnames(dd)[3:16], "入联系强度")
Str.out <- sapply(resnet, function(x){strength(x, mode = "out")})
colnames(Str.out) <- paste0(colnames(dd)[3:16], "出联系强度")
Str.all <- sapply(resnet, function(x){strength(x, mode = "all")})
colnames(Str.all) <- paste0(colnames(dd)[3:16], "总联系强度")

Str <- cbind(Str.in, Str.out, Str.all)
Str <- Str[unique(dd$c1),]
write.table(Str, "./res/Str.txt", sep = "\t", quote = F, col.names = NA)

#disparity
disp <- function(g){
  m1 <- get.adjacency(g, attr="weight")
  m2 <- m1^2
  return(((rowSums(m2)/(rowSums(m1)^2))*18-1)/17)
}

resd <- sapply(resnet, disp)
colnames(resd) <- paste0(colnames(dd)[3:16], "网络异质性")
resd <- resd[unique(dd$c1),]
resd[is.nan(resd)] <- 1
write.table(resd, "./res/resd.txt", sep = "\t", quote = F, col.names = NA)

# # 节点属性（不一定必须，netdraw中有计算中介中心性的功能）
# attr1 <- data.frame(name=V(g1)$name, betweenness=bet.all[,1])
# attr14 <- data.frame(name=V(g14)$name, betweenness=bet.all[,14])
# write.table(attr1, "./res/attr1.txt", sep = "\t", quote = F, col.names = NA)
# write.table(attr14, "./res/attr14.txt", sep = "\t", quote = F, col.names = NA)

# 节点邻接矩阵
adja1 <- igraph::get.adjacency(resnet[[1]], attr = "weight")
adja2 <- igraph::get.adjacency(resnet[[14]], attr = "weight")
write.table(as.matrix(adja1), "./res/adja1.txt", sep = "\t", quote = F, col.names = NA)
write.table(as.matrix(adja2), "./res/adja2.txt", sep = "\t", quote = F, col.names = NA)
