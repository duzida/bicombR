library(Matrix)
library(dplyr)
library(stringr)
library(readxl)
library(igraph)

#Create Results directory
dir.create("./res", showWarnings = FALSE, recursive = T)

# set global options
options(stringsAsFactors = FALSE)
# options(encoding="utf-8")
Sys.setlocale(category = "LC_ALL", locale = "us")
Sys.setlocale(category = "LC_ALL", locale = "cht")
#Error: invalid multibyte character in parser at line 1

dat <- read_xlsx("源数据.xlsx")

dat <- filter(dat, 提问者!="匿名用户")
ques <- unique(dat$问题)
usr <- unique(c(dat$提问者, dat$回答者, dat$评论人, dat$回复))
unique(dat$提问者)

match(c("知乎用户", ""), usr)

m1 <- Matrix(0, nrow = length(usr),ncol = length(usr), dimnames = list(usr, usr))

#提问回答
dat2 <- unique(dat[,1:3])
dat2[1:10,]
for(i in 1:nrow(dat2)){
  m1[as.character(dat2[i,c(1,3)]), as.character(dat2[i,c(1,3)])] <- 1 + 
    m1[as.character(dat2[i,c(1,3)]), as.character(dat2[i,c(1,3)])]
}

m1[1:5, 1:5]
# which(dat2$回答者=="大脑正常人" & dat2$提问者=="大脑正常人")

# 评论
dat3 <-  select(filter(dat, is.na(回复)), 1,3:6)
dat$回复[is.na(dat$回复)] <- dat$回答者[is.na(dat$回复)]

for(i in 1:nrow(dat)){
  m1[as.character(dat[i,c(4,6)]), as.character(dat[i,c(4,6)])] <- 1 +
    m1[as.character(dat[i,c(4,6)]), as.character(dat[i,c(4,6)])]
}
dat[1:10,c(1,3:6)]

#NA,知乎用户usr
m1 <- m1[-is.na(usr), -is.na(usr)]
m1 <- m1[-match("知乎用户", usr), -match("知乎用户", usr)]

m2 <- m1
diag(m2) <- 0
summary(rowSums(m2))
summary(colSums(m2))
which(rowSums(m2)==0)
m2 <- m2[-which(rowSums(m2)==0), -which(rowSums(m2)==0)]
write.table(as.matrix(m2), "res/adjamatrix.txt", col.names = NA, quote = F, sep = "\t")

head(sort(usr))
tail(sort(usr))

g <- graph_from_adjacency_matrix(as.matrix(m2), mode = "undirected", diag = F, weighted = T)
g
plot(g)
graph.density(g)
average.path.length(g)
igraph::transitivity(g)
degree.distribution(g)
mean(degree(g))
sd(degree(g))

table(summary(m2)$x)
summary(m2)[summary(m2)$x>20,]
resd1 <- as.data.frame(table(summary(m2)$x))
write.table(resd1, "res/频数.txt", col.names = NA, quote = F, sep = "\t")

subgraph(g,1)
subcomponent(g, 1)
igraph::induced_subgraph()

subconnet <- components(g)
dy <- as.data.frame(table(subconnet$membership))
write.table(resd1, "res/dy.txt", col.names = NA, quote = F, sep = "\t")
?subgraph
?components
?subcomponent
maxsubg <- subgraph(g, names(subconnet$membership)[subconnet$membership==1])
m3 <- get.adjacency(maxsubg)
write.table(as.matrix(m3), "res/最大连通子图.txt", col.names = NA, quote = F, sep = "\t")

names(sort(degree(g), decreasing = T)[1:30])
V(g)$name[V(g)$name=="张远卓"] <- "caiws"
V(g)$name[V(g)$name=="匿名用户"] <- "张远卓"
g2 <- subgraph(g,names(sort(degree(g), decreasing = T)[1:30]))
plot(g2)

g1 <- subgraph(g, subcomponent(g, "大脑正常人"))
g2 <- subgraph(g, subcomponent(g, "Vigorous Cooler"))
g3 <- subgraph(g, subcomponent(g, "豆子"))
g4 <- subgraph(g, subcomponent(g, "橘子"))
g5 <- subgraph(g, subcomponent(g, "张远卓"))
g2
g3

adjam <- get.adjacency(g)
adjam[1:5, 1:5]
names(which(adjam["大脑正常人",]!=0))
g2 <- subgraph(g, c("大脑正常人",names(which(adjam["大脑正常人",]!=0))))
g3 <- subgraph(g, c("Vigorous Cooler",names(which(adjam["Vigorous Cooler",]!=0))))
g4 <- subgraph(g, c("豆子",names(which(adjam["豆子",]!=0))))
g5 <- subgraph(g, c("橘子",names(which(adjam["橘子",]!=0))))
g6 <- subgraph(g, c(names(sort(degree(g), decreasing = T)[5]),names(which(adjam[names(sort(degree(g), decreasing = T)[5]),]!=0))))
g7 <- subgraph(g,names(sort(degree(g), decreasing = T)[1:100]))
g8 <- subgraph(g7,  subcomponent(g7, "大脑正常人"))
g2
g3
g4
g5
g6
g8
plot(g8)
write.table(as.matrix(get.adjacency(g8)), "res/subgg8.txt", col.names = NA, quote = F, sep = "\t")
zz <- list(g2,g3,g4,g5,g6)

sapply(1:5, function(x){
  testm <- get.adjacency(zz[[x]])
  write.table(as.matrix(testm), paste0("res/subgg",x,".txt"), col.names = NA, quote = F, sep = "\t")
})

ddd <- data.frame(name=V(g)$name, type=rep(2, 1193))
ddd$type[ match(V(g8)$name, V(g)$name)] <- 1
write.table(ddd, "res/ddd.txt", col.names = NA, quote = F, sep = "\t")
