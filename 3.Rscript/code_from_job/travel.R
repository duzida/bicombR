# Load libraries
library(igraph)
library(dplyr)
library(stringr)

#Create Results directory
dir.create("./res", showWarnings = FALSE, recursive = T)

# set global options
options(stringsAsFactors = FALSE)

d1 <- as.data.frame(readxl::read_xlsx("2014年经济联系度.xlsx"))
rownames(d1) <- d1$X__1
d1 <- d1[,-1]
d1 <- round(d1, 3)
write.table(d1, "d1.txt", sep = "\t", quote = F, col.names = NA)

d2 <- as.data.frame(readxl::read_xlsx("2017年经济联系度.xlsx"))
rownames(d2) <- d2$X__1
d2 <- d2[,-1]
d2 <- round(d2, 3)
write.table(d2, "d2.txt", sep = "\t", quote = F, col.names = NA)

d2 <- read.delim("d2.txt", header = T, row.names = 1)
d2
d1 <- read.delim("d1.txt", header = T, row.names = 1)
d1
d22 <- d2
d2[d2>0] <- 1
g1 <- graph.adjacency(as.matrix(d1), "undirected", weighted = T, diag = F)
g2 <- graph.adjacency(as.matrix(d2), "undirected", weighted = NULL, diag = F)

degree1 <- degree(g1)
close1 <- closeness(g1, )
betw1 <- betweenness(g1,)
res <- cbind(degree1, close1, betw1)

degree2 <- degree(g2)
close2 <- closeness(g2)
betw2 <- betweenness(g2)
res <- cbind(res,degree2, close2, betw2)
write.table(round(res,3), "cen.txt", sep = "\t", quote = F, col.names = NA)

density <- data.frame(class=c("2014年", "2017年"), density=round(c(graph.density(g1), graph.density(g2)),4))
write.table(density, "density.txt", sep = "\t", quote = F, col.names = NA)

ego