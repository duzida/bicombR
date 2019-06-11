# Load libraries
library(Matrix)
library(dplyr)
library(stringr)
library(igraph)

#Create Results directory
dir.create("./res", showWarnings = FALSE, recursive = T)

# set global options
options(stringsAsFactors = FALSE)
options(encoding="utf-8")

# data
# d1 <- readxl::read_xlsx("新建 Microsoft Excel 工作表.xlsx")
d1 <- readxl::read_xls("D5.xls")
d1 <- dplyr::select(d1, starts_with("申请人"))
l1 <- as.list(as.data.frame(t(d1)))
names(l1) <- 1:length(l1)
l1 <- sapply(l1, function(x){
  str_split(x, "; ")
})
# l1 <- sapply(l1, function(x){
#   tmp <- str_remove_all(x, ";")
#   tmp <- tmp[!is.na(tmp)]
# })



nodeterm <- unique(unlist(l1))

com <- Matrix(0, nrow = length(nodeterm), ncol = length(nodeterm), dimnames = list(nodeterm, nodeterm))
for (i in 1:length(l1)){
  com[l1[[i]], l1[[i]]] <- 1 + com[l1[[i]], l1[[i]]]
}

g <- graph_from_adjacency_matrix(com, mode = "undirected", weighted = T, diag = F)
g
plot(g, vertex.size=5, vertex.label = NA)


