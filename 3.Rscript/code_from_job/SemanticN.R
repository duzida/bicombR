# Load libraries
library(Matrix)
library(dplyr)
library(stringr)
library(igraph)

# set global options
options(stringsAsFactors = FALSE)
# options(encoding="utf-8")

# construct network

road_xh <- readxl::read_xlsx("road_xh.xlsx", col_names = T)
road_xh <- as.matrix(road_xh[,-1])
rownames(road_xh) <- colnames(road_xh)
road_xh[1:5, 1:5]


res1 <- as.tbl(as.data.frame(table(as.vector(road_xh))))%>% 
  arrange(desc(Freq))

res1 <- mutate(res1[-1,], freq2 = Freq/sum(Freq)*100, cumf = cumsum(Freq), cumf2 = cumsum(Freq)/sum(Freq)*100)
  

write.table(res1, "res1.txt", col.names = NA, sep = "\t", quote = F)




g_xh <- graph_from_adjacency_matrix(road_xh, mode = "undirected", weighted = T, diag = F)
g_xh


