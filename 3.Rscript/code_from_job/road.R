# Load libraries
library(readxl)
library(dplyr)
library(stringr)
library(igraph)
library(Matrix)
library(lsa)

# Create 01_Results directory
dir.create("./res", showWarnings = FALSE)

# set global options
options(stringsAsFactors = FALSE)

# 小数点会发生截断
# mat.fulldigits <- format(mat, digits = 20)
options(digits = 20)
lubiao <- read.delim("hfdian（节点坐标）.txt", header = F, row.names = NULL, quote = "'")
lubiao <- readxl::read_xlsx("路标.xlsx", sheet = 1, col_names = F, col_types = c("numeric", "numeric"))
head(lubiao)
colnames(lubiao) <- c("x", "y")

cosm <- lsa::cosine(t(as.matrix(lubiao)))
cosm[1:5, 1:5]

lukou <- unique(c(lubiao$from),c(lubiao$to))
lukou[1:10]

lubiao$value <- rep(1,nrow(lubiao))
lubiao[,1] <- as.character(lubiao[,1])
lubiao[,2] <- as.character(lubiao[,2])
tdm <- dcast(lubiao, from~to, value.var = "value")
tdm[is.na(tdm)]<- 0
tdm[1:8,1:6]
match("3543133.2",colnames(tdm))
length(unique(lubiao$from))
length(unique(lubiao$to))
intersect(unique(lubiao$from), unique(lubiao$to))

rownames(tdm) <- tdm$from
tdm <- tdm[,-1]
table(rowSums(tdm))
table(colSums(tdm))

lubiao$ID <- 1:nrow(lubiao)
which(rowSums(tdm)==2)
sapply(names(which(rowSums(tdm)==2)), function(x){
  tmp <- lubiao[lubiao$from %in% "12017544",]
  tmp$ID[]
})


m <- readxl::read_xlsx("权重矩阵（原始路网各边长度）.xlsx", col_names = F)
colnames(m) <- 1:978
m[m!=0]<- 1
# m <- as.matrix(m)
# m[lower.tri(m)] <- 0
m[1:5,1:5]
table(rowSums(m))
table(colSums(m))

#luduan 
# m <- data.frame(m, check.names = F)
m$from <- 1:978
road_data <- melt(m, id="from")
head(road_data)
road_data <- road_data[road_data$value!=0,]
length(which(m!=0))

road_data$variable <- as.character(road_data$variable)
road_data$variable <- as.integer(road_data$variable)
# road_data$from <- as.character(road_data$from)
rownames(road_data) <- NULL
road_data <- arrange(road_data, from,variable)%>% 
  rename(to=variable)
# road_data$roadID <- 1:1524


road_data <- group_by(road_data, from)%>% 
  mutate(from_count=n())%>% 
  ungroup()%>% 
  group_by(to)%>% 
  mutate(to_count=n())%>% 
  ungroup()

# road_data$ID <- 1:1524


road_data$ID <- 1:3048
road_data$roadID <- 0

for(i in 1:3047){
  for(j in (i+1):3048){
    
  }
}

table(road_data$from_count)
lubiao <- lubiao[,-c(3:4)]
options(digits = 11)
lubiao2 <- mutate_if(lubiao, is.character, as.numeric)

lubiao$index <- 1:978
road_data$from_x <- lubiao$x[road_data$from]
road_data$from_y <- lubiao$y[road_data$from]
road_data$to_x <- lubiao$x[road_data$to]
road_data$to_y <- lubiao$y[road_data$to]

road_data$x <- road_data$to_x - road_data$from_x
road_data$y <- road_data$to_y - road_data$from_y

road_data2 <- road_data


for(i in 1:978){
  tmp <- road_data[road_data$from==i,]
  index <- which(road_data$from==i)
  tmp$roadID <- i
  # if(tmp$from_count<=2){
  #   tmp$roadID <- i
  # }
  
  if(tmp$from_count >2){
    cosm <- abs(cosine(t(tmp[,12:13])))
    cosm[lower.tri(cosm)] <- 0
    diag(cosm) <- 0
    
    if(tmp$from_count <5){
      # tt <- which(cosm==max(cosm),arr.ind=T)
      tmp$roadID[-which(cosm==max(cosm),arr.ind=T)] <- paste0(i,"_2")
    }else{
      tt1 <- which(cosm==max(cosm),arr.ind=T)
      tt2 <- which(cosm==sort(cosm, decreasing = T)[2],arr.ind=T) 
      
      tmp$roadID[tt2] <- paste0(i,"_2")
      tmp$roadID[-c(tt2,tt1)] <- paste0(i,"_3")
      
    }
    
  }
  
  road_data$roadID[index] <- tmp$roadID
  
  # 
  # #丁字路口
  # 
  # if(tmp$from_count==3){
  #   cosm <- abs(cosine(t(tmp[,12:13])))
  #   cosm <- which.max(cosm[upper.tri(cosm)])
  #   
  #   if(cosm == 1){
  #     tmp$roadID[3] <- paste0(i,"_2")
  #   }
  #   
  #   if(cosm == 2){
  #     tmp$roadID[2] <- paste0(i,"_2")
  #   }
  #   
  #   if(cosm == 3){
  #     tmp$roadID[1] <- paste0(i,"_2")
  #   }
  #   
  # }
  # 
  # #十字路口
  # if(tmp$from_count == 4){
  #   cosm <- abs(cosine(t(tmp[,12:13])))
  #   cosm <- which.max(cosm[upper.tri(cosm)])
  #   
  #   if(cosm %in% c(1,6)){
  #     tmp$roadID[3:4] <- paste0(i,"_2")
  #   }
  #   
  #   if(cosm %in% c(2,5)){
  #     tmp$roadID[c(2,4)] <- paste0(i,"_2")
  #   }
  #   
  #   if(cosm %in% c(3,4)){
  #     tmp$roadID[c(1,4)] <- paste0(i,"_2")
  #   }
  # }
  # 
  # # 5叉路口
  # if(tmp$from_count == 5){
  #   cosm <- abs(cosine(t(tmp[,12:13])))
  #   cosm[lower.tri(cosm)] <- 0
  #   diag(cosm) <- 0
  #   cosm <- cosm[upper.tri(cosm)]
  #   order(cosm)
  #   cosm <- which.max()
  #   
  #   if(cosm %in% c(1,6)){
  #     tmp$roadID[3:4] <- paste0(i,"_2")
  #   }
  #   
  #   if(cosm %in% c(2,5)){
  #     tmp$roadID[c(2,4)] <- paste0(i,"_2")
  #   }
  #   
  #   if(cosm %in% c(3,4)){
  #     tmp$roadID[c(1,4)] <- paste0(i,"_2")
  #   }
  # }
}

for(i in 1:978){
  tmp <- dplyr::filter(road_data, from==i| to==i)
  which(tmp$roadID)
  index <- which(road_data$from==i)
}

ROAD <- unique(road_data$roadID)
road_data3 <- road_data
for(i in ROAD){
  tmp <- filter(road_data, roadID==i)
  # for(j in 1:nrow(tmp)){
  #   filter(road_data, from==tmp$to[j] &)
  # }
  
  if(nrow(tmp)!=0){
    tmp2 <- filter(road_data, from%in%tmp$to & to%in%tmp$from)
    road_data$roadID[road_data$roadID %in% tmp2$roadID] <- i
  }

  
}

length(unique(road_data$roadID))
road_data$roadID <- as.numeric(factor(road_data$roadID))
road_data

road_data3 <- select(road_data, from, to, roadID)

g <- graph.data.frame(road_data3,directed = F)
g
adjm <- igraph::get.adjacency(g,attr = "roadID", type = "upper")
adjm[1:5,1:5]
adjm <- adjm/2
write.table(as.matrix(adjm), "res/路口adjm.txt", col.names = NA, quote = F, sep = "\t")

road_data3
road_list <- plyr::dlply(road_data3, "roadID", .fun = function(x){
  x<-x[1:2]
  unique(as.vector(as.matrix(x)))
})

m <- Matrix(0,nrow=length(road_list), ncol=length(road_list))
for(i in 1:(nrow(m)-1)){
  for(j in (i+1):nrow(m))
    m[i, j] <- length(intersect(road_list[[i]], road_list[[j]]))
}
m[1:5,1:5]

# m <- m+t(m)
sum(m@x)
isSymmetric(m)
m[1:5,1:5]
colnames(m) <- names(road_list)
rownames(m) <- names(road_list)
write.table(as.matrix(m), "res/对偶adjm.txt", col.names = NA, quote = F, sep = "\t")
write.table(road_data[,c(1,2,7:11)], "res/原始数据.txt", col.names = T, row.names = F, quote = F, sep = "\t")

m2 <- m
m2[m>1] <- 1
g3 <- graph.adjacency(m2, mode = "undirected", diag = F, weighted = NULL)
g3

Degree <- degree(g3)
Closeness <- igraph::centralization.closeness(g3, normalized = T)$res
Betweenness <- centralization.betweenness(g3, directed = F)$res
Evect <- igraph::centralization.evcent(g3)$vector
CC <- sapply(V(g3)$name, function(x)igraph::transitivity(g3, vids = x))
distance <- shortest.paths(g3)
dd <- rowMeans(distance)

zhibiao <- cbind(ID=1:341, Degree, dd, CC, Closeness, Betweenness, Evect)
head(zhibiao)
# colnames(zhibiao) <- c("节点ID", "度数", "")
write.table(zhibiao, "res/总网指标.txt", col.names = T, row.names = F, quote = F, sep = "\t")
write.table(road_data3, "res/road_data3.txt", col.names = T, row.names = F, quote = F, sep = "\t")

?igraph::degree.distribution()
m <- read.delim("res/adjm.txt", header = T, row.names = 1, check.names = F)
m <- as.matrix(m)
degree.distribution(g3)
max(degree(g3))
options(digits = 7)
degreedis <- data.frame(k=1:69, pk=degree.distribution(g3)[-1])
degreedis <- filter(degreedis, pk!=0)
# degreedis <- round(degreedis,4)
library(ggplot2)
ggplot(degreedis, aes(x=k, y=pk))+geom_point(color="blue")+ylab("p(k)")+
  geom_line()+ggsave("res/总网度分布.png")

write.table(degreedis, "res/总网度分布.txt", col.names = T, row.names = F, quote = F, sep = "\t")

library(sna)
?efficiency()

dist <- igraph::distances(g3)
dist <- 1/dist
diag(dist) <- 0
dist[1:5,1:5]

ge <- sum(dist)/(341*340)

lubness <- data.frame(re=rep(0,341), r1=rep(0,341), r2=rep(0,341), 
                      r3=rep(0,341), r4=rep(0,341), r5=rep(0,341))

# d1 <- readxl::read_xlsx("res/指标.xlsx", sheet = 1)
zhibiao <- data.frame(zhibiao)
subgeff <- function(g, v){
  subg1 <- subgraph(g, (1:vcount(g))[-v])
  subg.dist1 <- 1/igraph::distances(subg1)
  diag(subg.dist1) <- 0
  subg.ge1 <- sum(subg.dist1)/(vcount(subg1)*(vcount(subg1)-1))
  return(subg.ge1/ge)
  
}

for(i in 1:340){
  lubness$re[i] <- i/341
  
  index1 <- sample(x = 1:341, size = i, replace = F)
  lubness$r1[i] <- subgeff(g3, index1)
  
  index2 <- arrange(zhibiao, desc(Degree))$ID[1:i]
  lubness$r2[i] <- subgeff(g3, index2)
  
  index3 <- arrange(zhibiao, desc(Closeness))$ID[1:i]
  lubness$r3[i] <- subgeff(g3, index3)
  
  index4 <- arrange(zhibiao, desc(Betweenness))$ID[1:i]
  lubness$r4[i] <- subgeff(g3, index4)
  
  index5 <- arrange(zhibiao, desc(Evect))$ID[1:i]
  lubness$r5[i] <- subgeff(g3, index5)
  
}

lubness[341,] <- c(1,0,0,0,0,0)
lubness[340,2:6] <- c(0,0,0,0,0)
# lubness <- lubness[1:37,]

library(ggplot2)
ggplot(lubness, aes(x=re, y=r1))+geom_point()+geom_line()+ 
  xlab("节点失效率")+ylab("网络相对效率")+ggsave("res/随机.png")

ggplot(lubness, aes(x=re, y=r3))+geom_point()+geom_line()+ 
  xlab("节点失效率")+ylab("网络相对效率")+ggsave("res/度数.png")

library(reshape2)
lubness2 <- melt(lubness[,1:3], id="re")
ggplot(lubness2, aes(x=re, y=value, color=variable))+geom_point()+geom_line()+ 
  xlab("节点失效率")+ylab("网络相对效率")+
  scale_color_discrete(labels=c("随机攻击", "基于度攻击"))+ 
  guides(color=guide_legend(title=NULL))+
  ggsave("res/度数.png")

lubness2 <- melt(lubness[,c(1,2,4)], id="re")
ggplot(lubness2, aes(x=re, y=value, color=variable))+geom_point()+geom_line()+ 
  xlab("节点失效率")+ylab("网络相对效率")+
  scale_color_discrete(labels=c("随机攻击", "基于接近\n中心度攻击"))+ 
  guides(color=guide_legend(title=NULL))+
  ggsave("res/接近中心度.png")

lubness2 <- melt(lubness[,c(1,2,5)], id="re")
ggplot(lubness2, aes(x=re, y=value, color=variable))+geom_point()+geom_line()+ 
  xlab("节点失效率")+ylab("网络相对效率")+
  scale_color_discrete(labels=c("随机攻击", "基于介数攻击"))+ 
  guides(color=guide_legend(title=NULL))+
  ggsave("res/介数.png")

lubness2 <- melt(lubness[,c(1,2,6)], id="re")
ggplot(lubness2, aes(x=re, y=value, color=variable))+geom_point()+geom_line()+ 
  xlab("节点失效率")+ylab("网络相对效率")+
  scale_color_discrete(labels=c("随机攻击", "基于特征\n向量攻击"))+ 
  guides(color=guide_legend(title=NULL))+
  ggsave("res/特征向量.png")

colnames(lubness) <- c("节点失效率","随机攻击","度数攻击","接近中心度攻击", 
                       "介数攻击","特征向量攻击")
write.table(lubness, sep = "\t", row.names = F, col.names = T, quote = F, "res/总网鲁棒性.txt")
ge

