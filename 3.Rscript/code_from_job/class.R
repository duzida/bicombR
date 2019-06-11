# Load libraries
library(dplyr)
library(igraph)
library(Matrix)
# library(reshape2)

# Create 01_Results directory
# dir.create("./res", showWarnings = FALSE)

# set global options
options(stringsAsFactors = FALSE)

?triad.census()

m <- readxl::read_xlsx("C班/zz.xlsx")
m[1:5,1:5]
m <- m[,-1]
write.table(diag(as.matrix(m)), row.names = T, col.names = F, "C班/表4-5.txt", sep="\t", quote=F)

m2 <- read.delim("C班/像矩阵.txt", header=F)
tmp <- mean(as.matrix(m2))
m2[m2>tmp] <- 1
m2[m2<tmp] <- 0
m2
colnames(m2) <- rownames(m2)
write.table(m2, col.names = NA, "C班/表4-8.txt", sep="\t", quote=F)
