# Load libraries
library(igraph)
library(dplyr)
library(stringr)

#Create Results directory
dir.create("./res", showWarnings = FALSE, recursive = T)

# set global options
options(stringsAsFactors = FALSE)


scienceNET <- function(id){
  d1 <- as.data.frame(readxl::read_xlsx("../data/跪求好好做.xlsx", sheet=id))
  rownames(d1) <- d1$X__1
  d1 <- d1[,-1]
  d1[is.na(d1)] <- 0
  
  g1 <- igraph::graph_from_incidence_matrix(as.matrix(d1), weighted = T, directed = F)
  
  adja <- get.adjacency(g1, attr = "weight")
  dir.create(paste0("../res/", id), showWarnings = FALSE, recursive = T)
  write.table(d1, paste0("../res/", id, "/", id, ".txt"), sep = "\t", quote = F, col.names = NA)
  write.table(as.matrix(adja), paste0("../res/", id, "/adja.txt"), sep = "\t", quote = F, col.names = NA)
  
  return(g1)
}

resnet <- lapply(1:20, scienceNET)

res1 <- sapply(resnet, function(x){
  
  t1 <- graph.density(x)
  t2 <- igraph::average.path.length(x)
  return(c(t1,t2))
})

write.table(t(res1), "../res/密度&距离.txt", sep = "\t", quote = F, col.names = NA)



# d1 <- as.data.frame(readxl::read_xlsx("ucinet kaishilelll.xlsx"))
# rownames(d1) <- d1$X__1
# d1 <- d1[,-1]
# d1[is.na(d1)] <- 0

# g1 <- graph_from_adjacency_matrix(as.matrix(d1), weighted = T)
# g1 <- igraph::graph_from_incidence_matrix(as.matrix(d1), weighted = T, directed = F)
# g1
# res <- data.frame(id = V(g1)$name)
# res$degree <- degree(g1)
# res$betweenness <- betweenness(g1)
# res$closeness <- closeness(g1)
# res$density <- graph.density(g1)
# res$eigen_centrality <- igraph::eigen_centrality(g1)$vector

# write.table(res, "./res/res.txt", sep = "\t", quote = F, col.names = T, row.names = F)
# 
# adja <- get.adjacency(g1, attr = "weight")
# write.table(as.matrix(adja), "./res/adja.txt", sep = "\t", quote = F, col.names = NA)
# 
# igraph::triad.census(g1)
# igraph::average.path.length(g1)
