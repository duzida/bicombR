# Load libraries
library(Matrix)
library(dplyr)
library(stringr)
library(readxl)
library(igraph)
library(ggplot2)
library(reshape2)

#Create Results directory
dir.create("./res", showWarnings = FALSE, recursive = T)

# set global options
options(stringsAsFactors = FALSE)
options(encoding="utf-8")#utf-无BOM格式才可以
# Sys.setlocale("LC_ALL","Chinese")

H1.e <- dir("多层复杂网络数据/HT1/出口/")
# 打开的excel文件在读取时也会存在，样如~$us_tradematrix_e_groups_2001.xlsx，
# 因此读取文件夹时，不能打开该文件夹内的任一文件
H1.i <- dir("多层复杂网络数据/HT1/进口/")

H1 <- lapply(1:17, function(x){
  setwd("多层复杂网络数据/HT1/出口/")
  t1 <- read_xlsx(H1.e[x])
  t1 <- t1[,-1]
  setwd("../进口/")
  t2 <- read_xlsx(H1.i[x])
  t2 <- t2[,-1]
  t2 <- t(t2)
  t3 <- (t1+t2)/2
  setwd("../../../")
  return(t3)
})


dclean <- function(file){
  H1.e <- dir(paste0("多层复杂网络数据/", file, "/出口/"))
  H1.i <- dir(paste0("多层复杂网络数据/", file, "/进口/"))
  
  H1 <- lapply(1:17, function(x){
    setwd(paste0("多层复杂网络数据/", file, "/出口/"))
    t1 <- read_xlsx(H1.e[x])
    t1 <- t1[,-1]
    setwd("../进口/")
    t2 <- read_xlsx(H1.i[x])
    t2 <- t2[,-1]
    t2 <- t(t2)
    t3 <- (t1+t2)/2
    setwd("../../../")
    return(t3)
  })
  
  return(H1)
}

H1 <-dclean("HT1")
# H1[[1]][1:5, 1:5]
# t3[1:5, 1:5]
H2 <-dclean(file ="HT2")

H <- lapply(1:17, function(x){
  (H1[[x]]+H2[[x]])/2
})

M1 <-dclean("MT1")
M2 <-dclean("MT2")
M3 <-dclean("MT3")

M <- lapply(1:17, function(x){
  (M1[[x]]+M2[[x]]+M3[[x]])/3
})

L1 <-dclean("LT1")
L2 <-dclean("LT2")
L <- lapply(1:17, function(x){
  (L1[[x]]+L2[[x]])/2
})

rm(x,t1,t2,t3)

# network construct
g_h1 <- graph_from_adjacency_matrix(as.matrix(H[[1]]), mode = "directed", weighted = T, diag = F)
g_h1

g_h <- lapply(1:17, function(x) graph_from_adjacency_matrix(as.matrix(H[[x]]), mode = "directed", weighted = T, diag = F))
g_h[[1]]
g_h[[2]]

g_m <- lapply(1:17, function(x) graph_from_adjacency_matrix(as.matrix(M[[x]]), mode = "directed", weighted = T, diag = F))
g_l <- lapply(1:17, function(x) graph_from_adjacency_matrix(as.matrix(L[[x]]), mode = "directed", weighted = T, diag = F))
g_m[[1]]
g_l[[1]]


# V(g_h[[1]])$name <- paste0(V(g_h[[1]])$name, "_h")
# V(g_h[[1]])$type <- "h"
# V(g_m[[1]])$name <- paste0(V(g_m[[1]])$name, "_m")
# V(g_m[[1]])$type <- "m"
# V(g_l[[1]])$name <- paste0(V(g_l[[1]])$name, "_l")
# V(g_l[[1]])$type <- "l"
# g11 <- igraph::graph.disjoint.union(g_h[[1]], g_m[[1]], g_l[[1]])
# V(g11)$type
# V(g11)$name
# 
# for(i in 1:50){
#   g11 <- g11 + path(paste0(V(g_h1)$name[i], c("_h", "_m", "_l")))
# }



g1 <- lapply(1:17, function(x){
  
  V(g_h[[x]])$name <- paste0(V(g_h[[x]])$name, "_h")
  V(g_h[[x]])$type <- "h"
  V(g_m[[x]])$name <- paste0(V(g_m[[x]])$name, "_m")
  V(g_m[[x]])$type <- "m"
  V(g_l[[x]])$name <- paste0(V(g_l[[x]])$name, "_l")
  V(g_l[[x]])$type <- "l"
  g_tmp <- igraph::graph.disjoint.union(g_h[[x]], g_m[[x]], g_l[[x]])
  
  for(i in 1:50){
    g_tmp <- g_tmp + path(paste0(V(g_h1)$name[i], c("_h", "_m", "_l")), weight=1)
  }
  
  return(g_tmp)
})

g2 <- lapply(1:3, function(y){
  g_tmp1 <- lapply(1:17, function(x){
    V(g_h[[x]])$name <- paste0(V(g_h[[x]])$name, "_", x)
    V(g_h[[x]])$type <- x
  })
})

g21 <- lapply(1:17, function(x){
  V(g_h[[x]])$name <- paste0(V(g_h[[x]])$name, "_", x)
  V(g_h[[x]])$type <- x
  return(g_h[[x]])
})

g21 <- igraph::graph.disjoint.union(g21)
g21

for(i in 1:50){
  g21 <- g21 + path(paste0(V(g_h1)$name[i], "_", 1:17), weight=1)
}

# m
g22 <- lapply(1:17, function(x){
  V(g_m[[x]])$name <- paste0(V(g_m[[x]])$name, "_", x)
  V(g_m[[x]])$type <- x
  return(g_m[[x]])
})
g22 <- igraph::graph.disjoint.union(g22)
for(i in 1:50){
  g22 <- g22 + path(paste0(V(g_h1)$name[i], "_", 1:17), weight=1)
}

# l
g23 <- lapply(1:17, function(x){
  V(g_l[[x]])$name <- paste0(V(g_l[[x]])$name, "_", x)
  V(g_l[[x]])$type <- x
  return(g_l[[x]])
})
g23 <- igraph::graph.disjoint.union(g23)
for(i in 1:50){
  g23 <- g23 + path(paste0(V(g_h1)$name[i], "_", 1:17), weight=1)
}

g2 <- list(g21,g22,g23)
g2[[1]]

# add_edges(c(1,51))、

# 3.network attri

sapply(1:17, function(x)length(E(g1[[x]])))
sapply(1:3, function(x)length(E(g2[[x]])))

g1[[2]]
g2[[1]]

# 强度
d1.out <- sapply(g1, function(x) strength(x, mode = "out"))
d1.in <- sapply(g1, function(x) strength(x, mode = "in"))
colnames(d1.out) <- paste0(2001:2017, ".out")
colnames(d1.in) <- paste0(2001:2017, ".in")
d1 <- cbind(d1.out,d1.in)
d1.out[1:10,]
d1[1:10,]

  
d2.out <- sapply(g2, function(x) strength(x, mode = "out"))
d2.in <- sapply(g2, function(x) strength(x, mode = "in"))
colnames(d2.out) <- c("H.out", "M.out", "L.out")
colnames(d2.in) <- c("H.in", "M.in", "L.in")
d2 <- cbind(d2.out,d2.in)
d2.out[1:10,]
d2[1:10,]

# 平均强度,平均出强度==平均入强度！！
# averD <- colMeans(d1)
# averD <- data.frame(strength=averD, type=rep(c("out", "in"), each=17), year=rep(2001:2017,times=2))
# ggplot(averD, aes(x=year, y=strength, shape=type))+geom_point()
averD <- colMeans(d1.in)
averD <- data.frame(strength=averD, year=2001:2017)
ggplot(averD, aes(x=year, y=strength))+ geom_point(size =5, color="blue")+ geom_line()+ 
  labs(x="年份", y="平均出强度")+ scale_x_discrete(limits=2001:2017)+ 
  ggsave("res/平均出强度.png" )

degree(g2[[1]], mode = "all")
degree.distribution(g2[[1]], cumulative = T)

sort(degree(g2[[1]]), decreasing = T)[1:5]

ddis <- as.data.frame(sapply(g2, function(x) degree.distribution(x, mode="out")))
colnames(ddis) <- c("H", "M", "L")
ddis$k <- 0:50
ddis <- melt(ddis, id.vars = "k", variable.name = "type", value.name = "pk")
head(ddis)

ggplot(ddis, aes(x=k, y=pk, color=type))+ geom_point()+ 
  geom_line()+ facet_grid(type~.)+ ggsave("res/度分布.png")

write.table(ddis, "ddis.txt", sep = "\t", col.names = NA, quote = F)
write.table(averD, "averD.txt", sep = "\t", col.names = NA, quote = F)

# 中心性
## g1
d2[1:10,]
c1.out <- sapply(g1, function(x) closeness(x, mode = "out"))
centr1 <- lapply(g1, function(x){
  stre1 <- strength(x, mode = "out")
  stre2 <- strength(x, mode = "in")
  clos1 <- closeness(x, mode= "out")
  clos2 <- closeness(x, mode= "in")
  betw1 <- betweenness(x, directed = T)
  node <- V(x)$name
  type <- V(x)$type
  # betw2 <- betweenness(x, )
  cbind(node, type,stre1, stre2, clos1, clos2, betw1)
})

centr2 <- lapply(g2, function(x){
  stre1 <- strength(x, mode = "out")
  stre2 <- strength(x, mode = "in")
  clos1 <- closeness(x, mode= "out")
  clos2 <- closeness(x, mode= "in")
  betw1 <- betweenness(x, directed = T)
  # node <- V(x)$name
  # type <- V(x)$type
  # betw2 <- betweenness(x, )
  cbind(stre1, stre2, clos1, clos2, betw1)
})

rm(centr, stre1, stre2, clos1, clos2, betw1)
centr1[[1]]
dim(centr1[[1]])

for(i in 1:17){
  centr1[[i]][,1] <- stringr::str_remove(centr1[[i]][,1], "_[hml]")
  centr1[[i]][,3:7] <- as.numeric(centr1[[i]][,3:7])
  year <- 2000+i
  centr1[[i]] <- cbind(year, centr1[[i]])
}
centr1[[1]][1:5,]

centr1 <- plyr::ldply(centr1)
write.table(centr1, "centr1.txt", sep = "\t", col.names = NA, quote = F)

as.tbl(centr1)
# topS.out <- group_by(centr1, year)%>% 
#   top_n(1, stre1)%>% 
#   select(1:4)
# 
# topS.in <- group_by(centr1, year)%>% 
#   top_n(1, stre2)%>% 
#   select(1:4)
# 
# topC.out <- group_by(centr1, year)%>% 
#   top_n(1, clos1)%>% 
#   select(1:4)
# 
# topC.in <- group_by(centr1, year)%>% 
#   top_n(1, clos2)%>% 
#   select(1:4)
# 
# topB <- group_by(centr1, year)%>% 
#   top_n(1, betw1)%>% 
#   select(1:4)
# 
# a <- top_n(centr1, 5, stre1)%>% 
#   select(1:4)
# b <- top_n(centr1, 5, stre2)%>% 
#   select(1:3,5)
# c <- top_n(centr1, 5, clos1)%>% 
#   select(1:3,6)
# d <- top_n(centr1, 5, clos2)%>% 
#   select(1:3,7)
# e <- top_n(centr1, 5, betw1)%>% 
#   select(1:3,8)
# 
# top_S <- cbind(a,b)
# top_C <- cbind(c,d)
# top_B <- e[1:5,]
# write.table(top_S, "top_S.txt", sep = "\t", col.names = NA, quote = F)
# write.table(top_C, "top_C.txt", sep = "\t", col.names = NA, quote = F)
# write.table(top_B, "top_B.txt", sep = "\t", col.names = NA, quote = F)

as.tbl(centr1)
top_cen <- melt(centr1, id.vars = c("year", "node", "type"), 
     variable.name = "measure_name", value.name = "centralization")
as.tbl(top_cen)
top_cen <- group_by(top_cen, measure_name)%>% 
  top_n(5, as.numeric(centralization))
top_cen

write.table(top_cen, "top_cen.txt", sep = "\t", col.names = NA, quote = F)

KNN <- igraph::knn(g2[[1]])
KNN
KNN.g1 <- sapply(g1, function(x){
  tmp<- knn(x)
  tmp[[2]]
})
  
KNN.g2 <- sapply(g2, function(x){
  tmp<- knn(x)
  tmp[[2]]
})

KNN.g2 <- as.data.frame(KNN.g2)
colnames(KNN.g2) <- c("H", "M", "L")
KNN.g2[1:10,]
KNN.g2$K <- 1:nrow(KNN.g2)
KNN.g2$M[1:5]
ggplot(KNN.g1[[1]], aes(x=K, y=M)) + geom_point()

t <- data.frame(knn=NULL, k=NULL, net=NULL)

for(i in 1:17){
  tmp <- as.data.frame(KNN.g1[[i]])
  tmp$k <- 1:nrow(tmp)
  colnames(tmp)[1] <- "knn"
  tmp$net <- i
  t <- rbind(t, tmp)
  # ggplot(t, aes(x=k, y=knn))+ geom_point()+ggsave(paste0("res/knn", i, ".png"))
}

t$net <- factor(t$net, labels = 2001:2017)


# ggplot(t, aes(x=k, y=knn, color=factor(net)))+ geom_point()+
#   facet_wrap(.~net)+ guides(color=F)+  ggsave("res/度相关.png", dpi=600)
#t least one layer must contain all variables used for facetting

ggplot(t, aes(x=k, y=knn, color=net))+ geom_point()+
  facet_wrap(~net)+ guides(color=F)+  ggsave("res/度相关2.png", dpi=600)

write.table(t, "t.txt", sep = "\t", col.names = NA, quote = F)

tranapl <- lapply(g1, function(x){
  t1 <- transitivity(x)
  t2 <- average.path.length(x)
  t <- data.frame(value=c(t1,t2), type=c("trans", "apl"))
  return(t)
})

tranapl[1]
tranapl <- plyr::ldply(tranapl)
tranapl$year <- rep(2001:2017, each=2)
tranapl <- dcast(tranapl, value.var = "value", formula = year~type)
colnames(tranapl) <- c("year", "average_path_length", "clustering_coefficient")

write.table(tranapl, "tranapl.txt", sep = "\t", col.names = NA, quote = F)

dir.create("ucinet")
adja2.h <- get.adjacency(g2[[1]], type = "both", attr = "weight")
adja2.m <- get.adjacency(g2[[2]], type = "both", attr = "weight")
adja2.l <- get.adjacency(g2[[3]], type = "both", attr = "weight")
adja2.h[1:5, 1:5]
write.table(as.matrix(adja2.h), "ucinet/adja2_h.txt", sep = "\t", col.names = NA, quote = F)
write.table(as.matrix(adja2.m), "ucinet/adja2_m.txt", sep = "\t", col.names = NA, quote = F)
write.table(as.matrix(adja2.l), "ucinet/adja2_l.txt", sep = "\t", col.names = NA, quote = F)

m <- HoltWinters(cusa)
p <- predict(m, 50, prediction.interval = TRUE)
plot(m, p)
# 
# index <- match(c("China", "United States of America"), colnames(H[[1]]))
tmp <- c()
for(i in 1:17){
  t1 <- t(H[[i]][8,])
  t2 <- t(M[[i]][8,])
  t3 <- t(L[[i]][8,])
  t <- cbind(t1,t2,t3)
  colnames(t) <- paste0(c("H_", "M_", "L_"), i)
  tmp <- cbind(tmp,t)
}
tmp[8:10,1:5]
tmp <- tmp[-8,]

tmp <- as.data.frame(tmp)
tmp.H <- select(tmp, starts_with("H"))
tmp.M <- select(tmp, starts_with("M"))
tmp.L <- select(tmp, starts_with("L"))

tmp.H[1:10,1:5]

year <- 1:17
cout <- as.numeric(tmp.H[1,])
m1 <- lm(cout~year)
predict(m1)

ch<- sapply(1:49, function(x){
  year <- 1:17
  cout <- as.numeric(tmp.H[x,])
  m1 <- lm(cout~year)
  predict(m1)[17]
})
cm<- sapply(1:49, function(x){
  year <- 1:17
  cout <- as.numeric(tmp.M[x,])
  m1 <- lm(cout~year)
  predict(m1)[17]
})
cl<- sapply(1:49, function(x){
  year <- 1:17
  cout <- as.numeric(tmp.L[x,])
  m1 <- lm(cout~year)
  predict(m1)[17]
})

Pred <- data.frame(H=ch, M=cm, L=cl)
rownames(Pred) <- rownames(tmp)

prdmly <- function(index){
  tmp <- c()
  for(i in 1:17){
    t1 <- t(H[[i]][index,])
    t2 <- t(M[[i]][index,])
    t3 <- t(L[[i]][index,])
    t <- cbind(t1,t2,t3)
    colnames(t) <- paste0(c("H_", "M_", "L_"), i)
    tmp <- cbind(tmp,t)
  }
  # tmp[8:10,1:5]
  # tmp <- tmp[-8,]
  
  tmp <- as.data.frame(tmp)
  tmp.H <- select(tmp, starts_with("H"))
  tmp.M <- select(tmp, starts_with("M"))
  tmp.L <- select(tmp, starts_with("L"))
  
  # tmp.H[1:10,1:5]
  
  year <- 1:17
  cout <- as.numeric(tmp.H[1,])
  m1 <- lm(cout~year)
  # predict(m1)
  
  ch<- sapply(1:50, function(x){
    year <- 1:17
    cout <- as.numeric(tmp.H[x,])
    m1 <- lm(cout~year)
    predict(m1)[17]
  })
  cm<- sapply(1:50, function(x){
    year <- 1:17
    cout <- as.numeric(tmp.M[x,])
    m1 <- lm(cout~year)
    predict(m1)[17]
  })
  cl<- sapply(1:50, function(x){
    year <- 1:17
    cout <- as.numeric(tmp.L[x,])
    m1 <- lm(cout~year)
    predict(m1)[17]
  })
  
  Pred <- data.frame(H=ch, M=cm, L=cl)
  rownames(Pred) <- rownames(tmp)
  Pred$node <- rownames(tmp)[index]
  
  return(Pred)
}

allmly <- prdmly(1)

allmly <- lapply(1:50, prdmly)
allmly <- plyr::ldply(allmly) 
allmly$rnode <- colnames(H[[1]])
allmly[c(1:5,51:55),]
  
Pred.H <- dcast(allmly, formula = rnode ~ node, value.var = "H")
Pred.M <- dcast(allmly, formula = rnode ~ node, value.var = "M")
Pred.L <- dcast(allmly, formula = rnode ~ node, value.var = "L")

rownames(Pred.H) <- Pred.H$rnode
Pred.H <- Pred.H[,-1]
# Pred.H <- as.matrix(Pred.H)
diag(Pred.H) <- 0
# diag(Pred.H) <- rowSums(Pred.H)

rownames(Pred.M) <- Pred.M$rnode
Pred.M <- Pred.M[,-1]
# Pred.M <- as.matrix(Pred.M)
diag(Pred.M) <- 0
# diag(Pred.M) <- rowSums(Pred.M)

rownames(Pred.L) <- Pred.L$rnode
Pred.L <- Pred.L[,-1]
# Pred.L <- as.matrix(Pred.L)
diag(Pred.L) <- 0
# diag(Pred.L) <- rowSums(Pred.L)

Pred.H[1:5, 1:5]
setwd("E://job/ucinet多层网络/")
write.table(Pred.H, "predictH.txt", sep = "\t", col.names = NA, quote = F)
write.table(Pred.M, "predictM.txt", sep = "\t", col.names = NA, quote = F)
write.table(Pred.L, "predictL.txt", sep = "\t", col.names = NA, quote = F)

Prank.H <- data.frame(country=rownames(Pred.H), out=rowSums(Pred.H), inport=colSums(Pred.H))
Prank.M <- data.frame(country=rownames(Pred.M), out=rowSums(Pred.M), inport=colSums(Pred.M))
Prank.L <- data.frame(country=rownames(Pred.L), out=rowSums(Pred.L), inport=colSums(Pred.L))

H_in <- top_n(Prank.H, 10, inport)%>% 
  select(country, inport)%>% 
  arrange(desc(inport))

H_out <- top_n(Prank.H, 10, out)%>% 
  select(country, out)%>% 
  arrange(desc(out))

M_in <- top_n(Prank.M, 10, inport)%>% 
  select(country, inport)%>% 
  arrange(desc(inport))

M_out <- top_n(Prank.M, 10, out)%>% 
  select(country, out)%>% 
  arrange(desc(out))

L_in <- top_n(Prank.L, 10, inport)%>% 
  select(country, inport)%>% 
  arrange(desc(inport))

L_out <- top_n(Prank.L, 10, out)%>% 
  select(country, out)%>% 
  arrange(desc(out))

H_in
M_in
L_in
cbind(H_in, M_in, L_in)

write.table(H_in, "H_in.txt", sep = "\t", col.names = NA, quote = F)
write.table(L_in, "L_in.txt", sep = "\t", col.names = NA, quote = F)
write.table(M_in, "M_in.txt", sep = "\t", col.names = NA, quote = F)
write.table(H_out, "H_out.txt", sep = "\t", col.names = NA, quote = F)
write.table(L_out, "L_out.txt", sep = "\t", col.names = NA, quote = F)
write.table(M_out, "M_out.txt", sep = "\t", col.names = NA, quote = F)


Prank.H[1:5,]

Pred.H[1:5, 1:5]
# Pred[47:49,]
# colnames(Pred) <- c("高技术产品", "中技术产品", "低技术产品")
# 
# write.table(Pred, "predict.txt", sep = "\t", col.names = NA, quote = F)

x <- Pred.H[,8]
Pred.H[,8] <- Pred.H[,48]
Pred.H[,8]
sum(Pred.H[,48])

#######
prdmly2 <- function(index){
  tmp <- c()
  for(i in 1:17){
    t1 <- H[[i]][,index]
    t2 <- M[[i]][,index]
    t3 <- L[[i]][,index]
    t <- cbind(t1,t2,t3)
    colnames(t) <- paste0(c("H_", "M_", "L_"), i)
    tmp <- cbind(tmp,t)
  }
  # tmp[8:10,1:5]
  # tmp <- tmp[-8,]
  
  tmp <- as.data.frame(tmp)
  tmp.H <- select(tmp, starts_with("H"))
  tmp.M <- select(tmp, starts_with("M"))
  tmp.L <- select(tmp, starts_with("L"))
  
  # tmp.H[1:10,1:5]
  
  year <- 1:17
  cout <- as.numeric(tmp.H[1,])
  m1 <- lm(cout~year)
  # predict(m1)
  
  ch<- sapply(1:50, function(x){
    year <- 1:17
    cout <- as.numeric(tmp.H[x,])
    m1 <- lm(cout~year)
    predict(m1)[17]
  })
  cm<- sapply(1:50, function(x){
    year <- 1:17
    cout <- as.numeric(tmp.M[x,])
    m1 <- lm(cout~year)
    predict(m1)[17]
  })
  cl<- sapply(1:50, function(x){
    year <- 1:17
    cout <- as.numeric(tmp.L[x,])
    m1 <- lm(cout~year)
    predict(m1)[17]
  })
  
  Pred <- data.frame(H=ch, M=cm, L=cl)
  rownames(Pred) <- rownames(tmp)
  Pred$node <- rownames(tmp)[index]
  
  return(Pred)
}

prdmly2(1)

allmly2 <- lapply(1:50, prdmly2)
allmly2 <- plyr::ldply(allmly2) 
allmly2$rnode <- colnames(H[[1]])
allmly2[c(1:5,51:55),]

Pred.H2 <- dcast(allmly2, formula = rnode ~ node, value.var = "H")
Pred.M2 <- dcast(allmly2, formula = rnode ~ node, value.var = "M")
Pred.L2 <- dcast(allmly2, formula = rnode ~ node, value.var = "L")


rownames(Pred.H2) <- Pred.H2$rnode
Pred.H2 <- Pred.H2[,-1]
diag(Pred.H2) <- 0

rownames(Pred.M2) <- Pred.M2$rnode
Pred.M2 <- Pred.M2[,-1]
diag(Pred.M2) <- 0

rownames(Pred.L2) <- Pred.L2$rnode
Pred.L2 <- Pred.L2[,-1]
diag(Pred.L2) <- 0

Pred.H2 <- read.delim("predictH.txt", row.names = 1)
Pred.M2 <- read.delim("predictM.txt", row.names = 1)
Pred.L2 <- read.delim("predictL.txt", row.names = 1)

Pred.H2 <- Pred.H2[,-1]
Pred.M2 <- Pred.M2[,-1]
Pred.L2 <- Pred.L2[,-1]

Pred.L2[1:5,1:5]

sum(Pred.H2[8,])


H_in
M_in
L_in


H[[18]] <- (H[[17]]+H[[16]])/2
rownames(H[[18]]) <- colnames(H[[18]])
M[[18]] <- (M[[17]]+M[[16]])/2
rownames(M[[18]]) <- colnames(M[[18]])
L[[18]] <- (L[[17]]+L[[16]])/2
rownames(L[[18]]) <- colnames(L[[18]])

Prank.H <- data.frame(country=colnames(H[[18]]), out=rowSums(H[[18]]), inport=colSums(H[[18]]))
Prank.M <- data.frame(country=colnames(M[[18]]), out=rowSums(M[[18]]), inport=colSums(M[[18]]))
Prank.L <- data.frame(country=colnames(L[[18]]), out=rowSums(L[[18]]), inport=colSums(L[[18]]))

H_in <- top_n(Prank.H, 10, inport)%>% 
  select(country, inport)%>% 
  arrange(desc(inport))

H_out <- top_n(Prank.H, 10, out)%>% 
  select(country, out)%>% 
  arrange(desc(out))

M_in <- top_n(Prank.M, 10, inport)%>% 
  select(country, inport)%>% 
  arrange(desc(inport))

M_out <- top_n(Prank.M, 10, out)%>% 
  select(country, out)%>% 
  arrange(desc(out))

L_in <- top_n(Prank.L, 10, inport)%>% 
  select(country, inport)%>% 
  arrange(desc(inport))

L_out <- top_n(Prank.L, 10, out)%>% 
  select(country, out)%>% 
  arrange(desc(out))

inport <- cbind(H_in, M_in, L_in)
out <- cbind(H_out, M_out, L_out)



write.table(out, "out.txt", sep = "\t", col.names = NA, quote = F)
write.table(inport, "in.txt", sep = "\t", col.names = NA, quote = F)
write.table(H[[18]], "18H.txt", sep = "\t", col.names = NA, quote = F)
write.table(M[[18]], "18M.txt", sep = "\t", col.names = NA, quote = F)
write.table(L[[18]], "18L.txt", sep = "\t", col.names = NA, quote = F)
