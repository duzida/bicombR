# Load libraries
library(igraph)
library(dplyr)
library(stringr)

#Create Results directory
dir.create("./01/res", showWarnings = FALSE, recursive = T)

# set global options
options(stringsAsFactors = FALSE)

#网络数据
node <- data.frame(name=c(paste0("I",1:16), paste0("D", 1:5), paste0("C", 1:4)), type=rep(c("a","b","c"), times=c(16,5,4)))
relation <- data.frame(from=node$name[-21], to=c(rep(node$name[17:21],time=c(4,4,4,4,8))))
g <- graph_from_data_frame(relation, directed=FALSE, vertices=node)
plot(g)

adja <- igraph::get.adjacency(g)
write.table(as.matrix(adja), "./01/res/adja.txt", sep = "\t", quote = F, col.names = NA)
write.table(node, "./01/res/attr.txt", sep = "\t", quote = F, col.names = NA)

#时效性
tmp <- igraph::distances(g)
T <- mean(1/tmp[upper.tri(tmp)])

#连通性
m <- igraph::graph.density(g)
apl <- igraph::average.path.length(g)
C <- m/apl

#鲁棒性
tmp <- degree(g)/sum(degree(g))
R <- -sum(tmp*log2(tmp))

#脆弱性
which.max(degree(g))
subg <- subgraph(g, V(g)$name[-21])
t=0.01
F <-  igraph::graph.density(subg)/igraph::graph.density(g) -t

#紧密型
Cl <- mean(closeness(g))

table1 <- data.frame("时效性"=T, "连通性"=C, "鲁棒性"=R, "脆弱性"=F, "紧密型"=Cl)
write.csv(table1, "01/res/表1_5.3指标.csv")

#随机网络
set.seed(1000)
randomnet <- function(p){
  g2 <- erdos.renyi.game(16, p)
  V(g2)$name <- paste0("I", 1:16)
  g2 <- g2 %>% 
    add_vertices(9, name=c(paste0("D",1:5), paste0("C", 1:4))) %>% 
    add_edges(c("I1","D1","I2","D1","I3","D1","I4","D1","I5","D2","I6","D2","I7","D2","I8","D2","I9","D3","I10","D3","I11","D3","I12","D3",
                "I13","D4","I14","D4","I15","D4","I16","D4","D1","D5","D2","D5","D3","D5","D4","D5","C1","D5","C2","D5","C3","D5","C4","D5"))
  return(g2)
}

res <- lapply(1:9/10, randomnet)
g2 <- res[[6]]
plot(g2)
adja2 <- igraph::get.adjacency(g2)
write.table(as.matrix(adja2), "./01/res/adja2.txt", sep = "\t", quote = FALSE, col.names = NA)

g3 <- g2 %>% 
  delete.vertices(c(paste0("D",1:5), paste0("C", 1:4)))
plot(g3)
adja3 <- igraph::get.adjacency(g3)
write.table(as.matrix(adja3), "./01/res/adja3.txt", sep = "\t", quote = FALSE, col.names = NA)

index <- function(net){
  #时效性
  tmp <- igraph::distances(net)
  T <- mean(1/tmp[upper.tri(tmp)])
  
  #连通性
  m <- igraph::graph.density(net)
  apl <- igraph::average.path.length(net)
  C <- m/apl
  
  #鲁棒性
  tmp <- degree(net)/sum(degree(net))
  R <- -sum(tmp*log2(tmp))
  
  #脆弱性
  subg <- subgraph(net, V(net)$name[- which.max(degree(net))])
  t=0.01
  F <-  igraph::graph.density(subg)/igraph::graph.density(net) -t
  
  #紧密型
  Cl <- mean(closeness(net))
  
  table <- data.frame("时效性"=T, "连通性"=C, "鲁棒性"=R, "脆弱性"=F, "紧密型"=Cl)
  return(table)
}

res2 <- sapply(res, index)
res2 <- t(res2)
write.table(as.matrix(res2), "./01/res/5结果.txt", sep = "\t", quote = FALSE, col.names = NA)

#最邻近耦合网络结构
?igraph::neighborhood(g)
?erdos.renyi.game
g4 <- igraph::make_ring(16)
plot(g4)
V(g4)$name <- paste0("I", 1:16)
g4 <- add_edges(g4,c(1,3,1,15,2,4,2,16,3,5,4,6,5,7,6,8,7,9,8,10,9,11,10,12,11,13,12,14,13,15,14,16))
adja4 <- igraph::get.adjacency(g4)
write.table(as.matrix(adja4), "./01/res/adja4.txt", sep = "\t", quote = FALSE, col.names = NA)

g5 <- g4 %>% 
  add_vertices(9, name=c(paste0("D",1:5), paste0("C", 1:4))) %>% 
  add_edges(c("I1","D1","I2","D1","I3","D1","I4","D1","I5","D2","I6","D2","I7","D2","I8","D2","I9","D3","I10","D3","I11","D3","I12","D3",
              "I13","D4","I14","D4","I15","D4","I16","D4","D1","D5","D2","D5","D3","D5","D4","D5","C1","D5","C2","D5","C3","D5","C4","D5"))
plot(g5)
adja5 <- igraph::get.adjacency(g5)
write.table(as.matrix(adja5), "./01/res/adja5.txt", sep = "\t", quote = FALSE, col.names = NA)
write.table(as.matrix(index(g5)), "./01/res/8结果.txt", sep = "\t", quote = FALSE, col.names = NA)


g6 <- graph_from_literal(I1:I2:I3:I4-I1:I2:I3:I4, I5:I6:I7:I8-I5:I6:I7:I8, I9:I10:I11:I12-I9:I10:I11:I12, I13:I14:I15:I16-I13:I14:I15:I16, 
                   I1-I16, I5-I2, I9-I6, I13-I10)
V(g6)$name <- paste0("I", 1:16)
plot(g6)

g7 <- g6 %>% 
  add_vertices(9, name=c(paste0("D",1:5), paste0("C", 1:4))) %>% 
  add_edges(c("I1","D1","I2","D1","I3","D1","I4","D1","I5","D2","I6","D2","I7","D2","I8","D2","I9","D3","I10","D3","I11","D3","I12","D3",
              "I13","D4","I14","D4","I15","D4","I16","D4","D1","D5","D2","D5","D3","D5","D4","D5","C1","D5","C2","D5","C3","D5","C4","D5"))
plot(g7)
adja7 <- igraph::get.adjacency(g7)
write.table(as.matrix(adja7), "./01/res/adja7.txt", sep = "\t", quote = FALSE, col.names = NA)
write.table(as.matrix(index(g7)), "./01/res/10结果.txt", sep = "\t", quote = FALSE, col.names = NA)

#同层级指挥控制节点网络协同
g8 <- add_edges(g7, unlist(sample(list(c("D1","D2"),c("D1","D3"),c("D1","D4"),c("D2","D3"),c("D2","D4"),c("D3","D4")), 2)))
g9 <- add_edges(g7, unlist(sample(list(c("D1","D2"),c("D1","D3"),c("D1","D4"),c("D2","D3"),c("D2","D4"),c("D3","D4")), 6)))
adja8 <- igraph::get.adjacency(g8)
write.table(as.matrix(adja8), "./01/res/adja8.txt", sep = "\t", quote = FALSE, col.names = NA)
adja9 <- igraph::get.adjacency(g9)
write.table(as.matrix(adja9), "./01/res/adja9.txt", sep = "\t", quote = FALSE, col.names = NA)

samplenet <- function(i){
  gg <- add_edges(g7, unlist(sample(list(c("D1","D2"),c("D1","D3"),c("D1","D4"),c("D2","D3"),c("D2","D4"),c("D3","D4")), i)))
  return(gg)
}

res3 <- lapply(1:6, samplenet)
res4 <- sapply(res3, index)
write.table(t(res4), "./01/res/12结果.txt", sep = "\t", quote = FALSE, col.names = NA)

#越级指挥控制节点网络协同
g10 <- add_edges(g9, unlist(sample(list(c("I1","D5"),c("I2","D5"),c("I3","D5"),c("I4","D5"),
                                 c("I5","D5"),c("I6","D5"),c("I7","D5"),c("I8","D5"), 
                                 c("I9","D5"),c("I10","D5"),c("I11","D5"),c("I12","D5"), 
                                 c("I13","D5"),c("I14","D5"),c("I15","D5"),c("I16","D5")), 6)))
plot(g10)

g11 <- add_edges(g9, unlist(sample(list(c("I1","D5"),c("I2","D5"),c("I3","D5"),c("I4","D5"),
                                        c("I5","D5"),c("I6","D5"),c("I7","D5"),c("I8","D5"), 
                                        c("I9","D5"),c("I10","D5"),c("I11","D5"),c("I12","D5"), 
                                        c("I13","D5"),c("I14","D5"),c("I15","D5"),c("I16","D5")), 16)))
plot(g11)
g11
adja10 <- igraph::get.adjacency(g10)
write.table(as.matrix(adja10), "./01/res/adja10.txt", sep = "\t", quote = FALSE, col.names = NA)
adja11 <- igraph::get.adjacency(g11)
write.table(as.matrix(adja11), "./01/res/adja11.txt", sep = "\t", quote = FALSE, col.names = NA)

samplenet2 <- function(i){
  gg <-add_edges(g9, unlist(sample(list(c("I1","D5"),c("I2","D5"),c("I3","D5"),c("I4","D5"),
                                        c("I5","D5"),c("I6","D5"),c("I7","D5"),c("I8","D5"), 
                                        c("I9","D5"),c("I10","D5"),c("I11","D5"),c("I12","D5"), 
                                        c("I13","D5"),c("I14","D5"),c("I15","D5"),c("I16","D5")), i)))
  return(gg)
}

res5 <- lapply(seq(2,16,2), samplenet2)
res6 <- sapply(res5, index)
write.table(t(res6), "./01/res/14结果.txt", sep = "\t", quote = FALSE, col.names = NA)

# plus1
g12 <- add_edges(g10, unlist(sample(list(c("C1","C2"),c("C1","C3"),c("C1","C4"),c("C2","C3"),c("C2","C4"),c("C3","C4")), 2)))
g13 <- add_edges(g10, unlist(sample(list(c("C1","C2"),c("C1","C3"),c("C1","C4"),c("C2","C3"),c("C2","C4"),c("C3","C4")), 6)))
plot(g12)
adja12 <- igraph::get.adjacency(g12)
write.table(as.matrix(adja12), "./01/res/adja12.txt", sep = "\t", quote = FALSE, col.names = NA)
adja13 <- igraph::get.adjacency(g13)
write.table(as.matrix(adja13), "./01/res/adja13.txt", sep = "\t", quote = FALSE, col.names = NA)

samplenet3 <- function(i){
  gg <- add_edges(g10, unlist(sample(list(c("C1","C2"),c("C1","C3"),c("C1","C4"),c("C2","C3"),c("C2","C4"),c("C3","C4")), i)))
  return(gg)
}
res7 <- lapply(1:6, samplenet3)
res8 <- sapply(res7, index)
write.table(t(res8), "./01/res/15结果.txt", sep = "\t", quote = FALSE, col.names = NA)

# plus2
samplenet4 <- function(i){ 
  gg <- add_edges(g13, unlist(sample(list(c("C1","I1"),c("C1","I2"),c("C1","I3"),c("C1","I4"),c("C1","I5"),c("C1","I6"),c("C1","I7"), 
                   c("C1","I8"),c("C1","I9"),c("C1","I10"),c("C1","I11"),c("C1","I12"),c("C1","I13"),
                   c("C1","I14"),c("C1","I15"),c("C1","I16"),c("C1","D1"),c("C1","D2"),c("C1","D3"),c("C1","I4"), 
                   c("C2","I1"),c("C2","I2"),c("C2","I3"),c("C2","I4"),c("C2","I5"),c("C2","I6"),c("C2","I7"), 
                   c("C2","I8"),c("C2","I9"),c("C2","I10"),c("C2","I11"),c("C2","I12"),c("C2","I13"),
                   c("C2","I14"),c("C2","I15"),c("C2","I16"),c("C2","D1"),c("C2","D2"),c("C2","D3"),c("C2","I4"), 
                   c("C3","I1"),c("C3","I2"),c("C3","I3"),c("C3","I4"),c("C3","I5"),c("C3","I6"),c("C3","I7"), 
                   c("C3","I8"),c("C3","I9"),c("C3","I10"),c("C3","I11"),c("C3","I12"),c("C3","I13"),
                   c("C3","I14"),c("C3","I15"),c("C3","I16"),c("C3","D1"),c("C3","D2"),c("C3","D3"),c("C3","I4"), 
                   c("C4","I1"),c("C4","I2"),c("C4","I3"),c("C4","I4"),c("C4","I5"),c("C4","I6"),c("C4","I7"), 
                   c("C4","I8"),c("C4","I9"),c("C4","I10"),c("C4","I11"),c("C4","I12"),c("C4","I13"),
                   c("C4","I14"),c("C4","I15"),c("C4","I16"),c("C4","D1"),c("C4","D2"),c("C4","D3"),c("C4","I4")), i*80)))
  return(gg)
}
res9 <- lapply(1:10/10, samplenet4)
res10 <- sapply(res9, index)
write.table(t(res10), "./01/res/16结果.txt", sep = "\t", quote = FALSE, col.names = NA)
plot(res9[[4]])
plot(res9[[10]])
adja14 <- igraph::get.adjacency(res9[[4]])
write.table(as.matrix(adja14), "./01/res/adja14.txt", sep = "\t", quote = FALSE, col.names = NA)
adja15 <- igraph::get.adjacency(res9[[10]])
write.table(as.matrix(adja15), "./01/res/adja15.txt", sep = "\t", quote = FALSE, col.names = NA)
res9[[4]]
