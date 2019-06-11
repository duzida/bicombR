# Load libraries
library(Matrix)
library(dplyr)
library(stringr)
library(igraph)
library(readxl)
library(xlsx) 

#Create Results directory
dir.create("./res", showWarnings = FALSE, recursive = T)

# set global options
options(stringsAsFactors = FALSE)
options(encoding="utf-8")#utf-无BOM格式才可以
# Sys.setlocale("LC_ALL","Chinese")
Sys.setlocale(category = "LC_ALL", locale = "us")
Sys.setlocale(category = "LC_ALL", locale = "cht")
#Error: invalid multibyte character in parser at line 1

dat <- read_xlsx("1.xlsx")
d1 <- dat[,1:3]
d1 <- filter(d1, !is.na(`2006`))%>% 
  select(1:2)
# d1[,1] <- as.numeric(d1[,1])
d1
m1 <- Matrix(0, nrow=length(unique(as.vector(t(d1[,1])))), ncol=length(unique(as.vector(t(d1[,2])))), 
             dimnames = list(c(unique(as.character(t(d1[,1])))), c(unique(as.vector(t(d1[,2]))))))
m1 
for(i in 1:nrow(d1)){
  m1[as.character(d1[i,1]), as.character(d1[i,2])] <- 1
}
m1 <- t(m1) %*% m1
# graph.incidence(m1, mode = "all")
g1 <- graph.adjacency(m1, mode = "undirected", weighted = T, diag = F)
degree(g1, normalized = T)
closeness(g1, normalized = T)
betweenness(g1, normalized = T)
res <- cbind(degree(g1), closeness(g1), betweenness(g1))
res

plot(g1)
datname <- colnames(dat)
index <- match("2007", datname)
dat[,index:(index+2)]

money <- function(year){
  index <- match(year, datname)
  dt <- dat[,index:(index+2)]
  dt <- filter(dt, !is.na(dt[,1]))%>% 
    select(1:2)
  
  mt <- Matrix(0, nrow=length(unique(as.vector(t(dt[,1])))), ncol=length(unique(as.vector(t(dt[,2])))), 
               dimnames = list(c(unique(as.character(t(dt[,1])))), c(unique(as.vector(t(dt[,2]))))))
  
  for(i in 1:nrow(dt)){
    mt[as.character(dt[i,1]), as.character(dt[i,2])] <- 1
  }
  mt <- t(mt) %*% mt
 
  gt <- graph.adjacency(mt, mode = "undirected", weighted = T, diag = F)
  
  res <- cbind(degree(gt), closeness(gt), betweenness(gt))
  colnames(res) <- c("degree", "closeness", "betweenness")
  write.table(res, paste0("res/", year, ".txt"), sep = "\t", col.names = NA, quote = F)
  
  return(res)
}

index <- match("2008", datname)
d1 <- dat[,index:(index+2)]
d1 <- filter(d1, !is.na(d1[,1]) & !is.na(d1[,2]))%>% 
  select(1:2)

length(unique(d1$X__5))

index <- match("2009", datname)
d2 <- dat[,index:(index+2)]
d2 <- filter(d2, !is.na(d2[,1]) & !is.na(d2[,2]))%>% 
  select(1:2)
colnames(d1) <- colnames(d2)
d2 <- rbind(d1,d2)
length(unique(d2$X__7))
dt <- d2

money2 <- function(year){
  index <- match(year, datname)
  dt <- dat[,index:(index+2)]
  dt <- filter(dt, !is.na(dt[,1]) & !is.na(dt[,2]))%>% 
    select(1:2)
  
  index2 <- match(year-1, datname)
  dt2 <- dat[,index2:(index2+2)]
  dt2 <- filter(dt2, !is.na(dt2[,1]) & !is.na(dt2[,2]))%>% 
    select(1:2)
  colnames(dt2) <- colnames(dt)
  
  dt <- rbind(dt, dt2)
  
  
  mt <- Matrix(0, nrow=length(unique(as.vector(t(dt[,1])))), ncol=length(unique(as.vector(t(dt[,2])))), 
               dimnames = list(c(unique(as.character(t(dt[,1])))), c(unique(as.vector(t(dt[,2]))))))
  
  for(i in 1:nrow(dt)){
    mt[as.character(dt[i,1]), as.character(dt[i,2])] <- 1
  }
  mt <- t(mt) %*% mt
  
  gt <- graph.adjacency(mt, mode = "undirected", weighted = T, diag = F)
  
  res <- cbind(degree(gt), closeness(gt), betweenness(gt))
  colnames(res) <- c("degree", "closeness", "betweenness")
  write.table(res, paste0("res/", year, ".txt"), sep = "\t", col.names = NA, quote = F)
  
  return(dt)
}

d3 <- money2(2009)

money3 <- function(year, ddd){
  index <- match(year, datname)
  dt <- dat[,index:(index+2)]
  dt <- filter(dt, !is.na(dt[,1]) & !is.na(dt[,2]))%>% 
    select(1:2)
  
  # index2 <- match(year-1, datname)
  # dt2 <- dat[,index2:(index2+2)]
  # dt2 <- filter(dt2, !is.na(dt2[,1]) & !is.na(dt2[,2]))%>% 
  #   select(1:2)
  
  colnames(ddd) <- colnames(dt)
  
  dt <- rbind(dt, ddd)
  
  
  mt <- Matrix(0, nrow=length(unique(as.vector(t(dt[,1])))), ncol=length(unique(as.vector(t(dt[,2])))), 
               dimnames = list(c(unique(as.character(t(dt[,1])))), c(unique(as.vector(t(dt[,2]))))))
  
  for(i in 1:nrow(dt)){
    mt[as.character(dt[i,1]), as.character(dt[i,2])] <- 1
  }
  mt <- t(mt) %*% mt
  
  gt <- graph.adjacency(mt, mode = "undirected", weighted = T, diag = F)
  
  res <- cbind(degree(gt), closeness(gt), betweenness(gt))
  colnames(res) <- c("degree", "closeness", "betweenness")
  write.table(res, paste0("res/", year, ".txt"), sep = "\t", col.names = NA, quote = F)
  
  return(dt)
}

d4 <- money3(2010, d3)
d5 <- money3(2011, d4)
d6 <- money3(2012, d5)
d7 <- money3(2013, d6)
d8 <- money3(2014, d7)
d9 <- money3(2015, d8)
d10 <- money3(2016, d9)
d11 <- money3(2017, d10)