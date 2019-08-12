# Load libraries
library(dplyr)
library(stringr)
library(XML)
library(plotly)
library(Matrix)

# Create 01_Results directory
dir.create("./res", showWarnings = FALSE)

# set global options
options(stringsAsFactors = FALSE)

# read data
setwd("./1.query/notefirst/")
cnkifilename <- dir()[str_detect(dir(),"CNKI-")]

# notefirst format
# 为排除同名现象，加上单位
# 作者与单位无法匹配，会存在作者3个单位2个的情况
# cnki中，不同文献类型节点标签是不一样的
# 作者分隔符有的是","
# cnki在关键词有的是英文，大小写得统一，空格不能去除


setClass("cnkiAB", representation(ti = "character", year = "character", type = "character", au = "list", 
                                  kw = "list", organization = "list", journal = "character", publisher = "character"))

cnki.parser <- function(file){
  
  # doc <- xmlTreeParse(file, useInternalNodes = T, encoding = "UTF-8")
  
  Parser <- function(xmlnodeset, path){
    sapply(xmlnodeset, function(x){
      temp = xpathSApply(x, path, xmlValue)
      if(length(temp)==0){
        return("NO RES")
      }else{
        if(temp==""){
          return("NO RES")
        }else{
          return(temp)
        }
      }
    })
  }
  
  Bibliography <- unlist(sapply(file, function(x){
    doc <- xmlTreeParse(x, useInternalNodes = T, encoding = "UTF-8")
    Bibliography <- getNodeSet(doc, "//Bibliography")
    return(Bibliography)
  }))
  
  # Bibliography <- getNodeSet(doc, "//Bibliography")
  
  Ti <- Parser(Bibliography, ".//PrimaryTitle//Title[@Lang='zh-CHS']")
  names(Ti) <- NULL
  
  Year <- Parser(Bibliography, ".//Year")
  names(Year) <- NULL
  
  Type <- Parser(Bibliography, ".//Type")
  names(Type) <- NULL
  
  Publisher <- Parser(Bibliography, ".//Publisher")
  names(Publisher) <- NULL
  
  AuList <- str_remove_all(Parser(Bibliography, ".//Authors//FullName"), "\\n")
  AuList <- str_remove_all(AuList, ";$")
  AuList <- str_split(AuList, ";|,")
  
  # AuList <- str_split(str_remove(Parser(Bibliography, ".//Authors//FullName"), ";$|\\n"), ";")
  
  OrgList <- str_split(str_remove(Parser(Bibliography, ".//Authors//Organization"), ";$"), ";")
  
  KwList <- str_split(str_remove(Parser(Bibliography, ".//Keywords//Keyword[@Lang='zh-CHS']"), ";$"), ";;")
  
  KwList <- str_split(KwList, ";")
  
  KwList <- sapply(KwList, function(x)str_to_lower(x))
  
  # KwList <- sapply(KwList, function(x)str_remove_all(x, " "))
  
  TEST = sd(c(length(Ti), length(Year), length(AuList), length(OrgList), length(KwList)))
  
  if(TEST == 0){
    profile <- new("cnkiAB", ti = Ti, year = Year, type = Type, 
                   au = AuList, organization = OrgList,  kw = KwList, publisher = Publisher)
    return(profile)
  }else return("XML Document Error")
}
cnkifilename <- dir("1.query/NoteFirst/")
setwd("./1.query/NoteFirst/")
cnkiAB <- cnki.parser(cnkifilename)
setwd("../../")
getwd()

table(cnkiAB@publisher)
table(cnkiAB@type)
table(cnkiAB@year)

any(cnkiAB@ti=="NO RES")
any(cnkiAB@type=="NO RES")
any(cnkiAB@au=="NO RES")# 标题,文献类型,作者节点标签是一致的

table(cnkiAB@type,cnkiAB@year)
table(cnkiAB@type[cnkiAB@organization=="NO RES"])
cnkiAB@ti[cnkiAB@type=="Newspaper"]
which(cnkiAB@kw %in% c("峰均功率比","正交频分复用","空间光调制器","峰均比","全息"))

which(sapply(cnkiAB@kw, function(x) any(x %in% c("峰均功率比","正交频分复用","空间光调制器","峰均比","全息"))))
which(sapply(cnkiAB@au, length)-sapply(cnkiAB@organization, length)!=0)
cnkiAB@au[6:7]
cnkiAB@organization[6:7]
### thsis，newspaper不同类型的文献需要不同的元素标签

# 删除不符合主题的关键词
index <- which(sapply(cnkiAB@kw, function(x) any(x %in% c("峰均功率比","正交频分复用","液晶空间光调制器",
                                                 "空间光调制器","峰均比","全息","ofdm","光学制造","激光光学"))))
# 合并同义词
cnkiAB@kw <- sapply(cnkiAB@kw, function(x){
  x <- str_replace_all(x, "激光选区熔化\\(slm\\)", "激光选区熔化")
  x <- str_replace_all(x, "选择性激光熔化|选区激光熔化|selective laser melting|slm|激光选区熔化技术|激光选区熔化成形", "激光选区熔化")
  x <- str_replace_all(x, "additive manufacturing", "增材制造")
  x <- str_replace_all(x, "微观组织|microstructure|微观结构", "显微组织")
  x <- str_replace_all(x, "选择映射", "选择性映射")
  x <- str_replace_all(x, "快速成形", "快速成型")
  x <- str_replace_all(x, "alsi10mg$", "alsi10mg合金")
  x <- str_replace_all(x, "mechanical properties", "机械性能")
  x <- str_remove_all(x, "of|in|as|on|for")
  x <- x[x!=""]
  x <- unique(x)
})



# 将文献类型为Newspaper的筛除
# 将无作者无关键词的文献剔除
# index <- which(cnkiAB@au!="NO RES" & cnkiAB@kw!="NO RES" & cnkiAB@type!="Newspaper")
# index <- which(cnkiAB@publisher != "情报学报" & cnkiAB@year !="2018")

cnki <- new("cnkiAB", ti = cnkiAB@ti[-index], year = cnkiAB@year[-index], 
            type = cnkiAB@type[-index], au = cnkiAB@au[-index], 
            organization = cnkiAB@organization[-index],  kw = cnkiAB@kw[-index], publisher = cnkiAB@publisher[-index])
table(cnki@type)
# cnki <- cnkiAB

# 频次表及可视图
# 年代
d1 <- as.tbl(as.data.frame(table(cnki@year)))

p1 <-  d1 %>% 
  plot_ly(x = ~Var1, y = ~Freq, type = "bar", text = ~Freq, textposition = 'outside', textfont = list(size=12)) %>% 
  layout(xaxis = list(title = "year", tickangle = -45, titlefont = list(size=18), tickfont = list(size=10)),
         yaxis = list(title = "the number of article", titlefont = list(size=18)), 
         margin = list(b = 100), 
         showlegend = FALSE) 

print(p1)
write.table(d1, "res/year_freq.txt", row.names = F, quote = F, sep = "\t")


# 核心作者
d3 <- as.tbl(as.data.frame(table(unlist(cnki@au)))) %>% 
  arrange(desc(Freq)) %>% 
  dplyr::rename(Author=Var1)
write.table(d3, "./res/author_freq.txt", row.names = F, quote = F, sep = "\t")

d3 <- d3[d3$Freq >= round(0.749*d3$Freq[1]^0.5),] %>% #2.247
  dplyr::rename(CoreAuthor=Author)
write.table(d3, "./res/core_author_freq.txt", row.names = F, quote = F, sep = "\t")

# 期刊
d2 <- as.tbl(as.data.frame(table(cnki@publisher, cnki@year)))%>% 
  arrange(desc(Var1))

d3 <- reshape2::dcast(d2, Var1~Var2, value.var = "Freq")
rownames(d3) <- d3$Var1
d3 <- d3[,-1]
d3$'总计' <- rowSums(d3)
d3 <- rbind(d3, colSums(d3))
rownames(d3)[10] <- "总计"
d3
write.table(d3, "./res/journal.txt", col.names = NA, quote = F, sep = "\t")

# 高频词
for(i in 1:length(cnki@kw)){
  cnki@kw[[i]] <- str_replace(cnki@kw[[i]], "《图书情报工作》", "图书情报工作")
}

cnki@kw <- sapply(cnki@kw, function(x){
  x <- str_replace_all(x, "激光选区熔化\\(激光选区熔化\\)", "激光选区熔化")
  # x <- str_replace_all(x, "快速成形", "快速成型")
  # x <- str_replace_all(x, "微观组织|microstructure|微观结构", "显微组织")
  # x <- str_replace_all(x, "选择映射", "选择性映射")
  # x <- str_replace_all(x, "alsi10mg", "alsi10mg合金")
  x <- unique(x)
})

d5 <- as.tbl(as.data.frame(table(unlist(cnki@kw)), stringsAsFactors = F)) %>% 
  arrange(desc(Freq)) %>% 
  dplyr::rename(Keyword=Var1)
write.table(d5, "./6.res/keyword/关键词统计.txt", row.names = F, quote = F, sep = "\t")


p2 <- as.data.frame(table(table(unlist(cnkiAB@kw)))) %>% 
  dplyr::mutate(cumfreq=(cumsum(Freq)/sum(Freq))*100) %>% 
  plot_ly(x=~Var1, y=~cumfreq, type = 'scatter', mode = 'lines+markers') %>% 
  layout(xaxis = list(title = "the freq of keyword", tickangle = -45),
         yaxis = list(title = "the cum_freq of \n article(100%)"), 
         margin = list(b = 100), 
         showlegend = FALSE)
  # add_trace() 
  
print(p2)

d5_cum <- as.data.frame(table(table(unlist(cnki@kw)))) %>% 
  dplyr::mutate(cumfreq=(cumsum(Freq)/sum(Freq))*100)

write.table(d5_cum, "./res/高频词累积.txt", row.names = F, quote = F, sep = "\t")

# threshold 选取
minfreq.df <- as.tbl(as.data.frame(table(table(unlist(cnkiAB@kw))), stringsAsFactors = F)) %>% 
  arrange(desc(as.integer(Var1))) %>% 
  dplyr::mutate(g1=cumsum(as.integer(Freq)*as.integer(Var1))) %>% 
  dplyr::mutate(g2=cumsum(as.integer(Freq))^2)

minfreq <- as.integer(with(minfreq.df, Var1[which(g1 < g2)[1]-1]))
d5_height <- d5[d5$Freq>minfreq,]

translate <- readxl::read_xlsx("res/高频词中英文对照.xlsx", col_names = F, sheet = 1)
word <- translate$X__1
d6 <- filter(d5, Keyword %in% word)
translate$Freq <- d5$Freq[match(translate$X__1, d5$Keyword)]
translate <- arrange(translate, desc(Freq))
colnames(translate) <- c("高频关键词_中文", "高频关键词_韩语", "关键词频次")
write.table(translate, "./res/高频关键词.txt", row.names = F, 
            quote = F, sep = "\t", fileEncoding = "utf-8")


#高频词词云
# p5 <- wordcloud2::wordcloud2(d5_height)
# print(p5)
write.table(d5, "./res/keyword_freq.txt", row.names = F, quote = F, sep = "\t")
write.table(d5_height, "./res/高频词.txt", row.names = F, quote = F, sep = "\t")

# 矩阵
term <- d5$Keyword
term.list <- cnki@kw
names(term.list) <- 1:length(term.list)

if(minfreq != 1){
  df <- as.tbl(as.data.frame(table(unlist(term.list)), stringsAsFactors = F))
  # term <- df$Var1[df$Freq >= minfreq]
  term <- word
  term.list <- sapply(term.list, function(x)return(intersect(x, term)))
  term.list <- term.list[sapply(term.list, length)!=0]
}

tdm <- Matrix(0, nrow = length(term), ncol = length(term.list), dimnames = list(term, names(term.list)))
for (i in 1:length(term.list)){
  tdm[term.list[[i]], i] <- 1
}
write.table(as.matrix(tdm), "./res/tdm.txt", sep = "\t", quote = F, col.names = NA)


com <- Matrix(0, nrow = length(term), ncol = length(term), dimnames = list(term, term))
for (i in 1:length(term.list)){
  com[term.list[[i]], term.list[[i]]] <- 1 + com[term.list[[i]], term.list[[i]]]
}
write.table(as.matrix(com), "./res/com.txt", sep = "\t", quote = F, col.names = NA)

rownames(com)
com2 <- com
com2["图书馆",] <- com2["公共图书馆",]
com2["移动图书馆",] <- com["移动服务",]
com2["文献传递",] <- com2["馆际互借",]

# com2["移动图书馆",] <- apply(com[c("移动图书馆","移动服务"),],2,mean)


write.table(as.matrix(com2), "./res/comm.txt", sep = "\t", quote = F, col.names = NA)



library(igraph)
identical(translate$X__1, rownames(com))
rownames(com)[1:5]
translate[1:5,]
translate$X__2[match(rownames(com), translate$X__1)][c(2,3,22,55)] <- 
  c("CALIS","MOOC","Interlibrary Loan","Wisdom Library")

# iconv(Encoding(translate$X__2[match(rownames(com), translate$X__1)]), from = "utf-8",
      # to="utf-8")
# Encoding(translate$X__2[match(rownames(com), translate$X__1)]) <- "UTF-8"
# dimnames(com) <- list(translate$X__2[match(rownames(com), translate$X__1)], 
                      # translate$X__2[match(rownames(com), translate$X__1)])

com2 <- com[-match(c("图书馆","大学图书馆"), rownames(com)), -match(c("图书馆","大学图书馆"), rownames(com))]

g <- graph.adjacency(com2, mode = "undirected", weighted = T, diag = F)
g
write.graph(g, "res/network.txt", "edgelist", directed=F)
degree <- data.frame(degree(g))
degree$vertex <- rownames(degree)
degree <- arrange(degree,desc(degree.g.))
degree$vertex[51:55]

network <- igraph::get.data.frame(g, what = "edges")
write.table(network, "./res/network.txt", sep = "\t", 
            quote = F, col.names = T, row.names = F)

dim(com2)
cluster <- readxl::read_xlsx("res/toclient/SPSS 聚类结果.xlsx")
cluster <- cluster[,c(2,4)]
diag(com2) <- 0
# 战略坐标 <- data.frame(密度=rep(0,6), 向心度=rep(0,6))
战略坐标 <- sapply(1:6, function(i){
  tmp1 <- mean(com2[cluster$高频关键词_中文[cluster$所属类别==i],cluster$高频关键词_中文[cluster$所属类别==i]])
  tmp2 <- mean(com2[cluster$高频关键词_中文[cluster$所属类别==i],cluster$高频关键词_中文[cluster$所属类别!=i]])
  return(c(tmp1,tmp2))
})

战略坐标 <- t(战略坐标)
colnames(战略坐标) <- c("密度","向心度")
rownames(战略坐标) <- paste0("类团", 1:6)
write.table(战略坐标, "./res/toclient/战略坐标.txt", sep = "\t", 
                quote = F, col.names = NA)

library(ggplot2)
战略坐标 <- data.frame(战略坐标)
战略坐标$cluster <- paste0("cluster", 1:6)
colnames(战略坐标) <- c("density", "centralization", "cluster")
ggplot(战略坐标, aes(x=density, y=centralization, color=cluster))+ geom_point(size=3,aes(shape=cluster))+ 
  ylim(c(0.2,0.6))+
  geom_vline(aes(xintercept=mean(density)))+geom_hline(aes(yintercept=mean(centralization)))+ 
  ggsave("res/toclient/战略坐标图.png")
  # guides(color=FALSE)
# + geom_text(aes(label=cluster),colour="black",size=1)

#
cluster <- readxl::read_xlsx("res/toclient/SPSS 聚类结果.xlsx")
t <- cluster$所属类别[match(cluster$vertex, cluster$高频关键词_韩文)]
t <- factor(t, labels = c("red", "yellow", "blue", "black", "lightblue", "green"))
write.table(t,"./res/t.txt", sep = "\t", 
                quote = F, col.names = F, row.names = F)

write.table(translate,"./res/translate.txt", sep = "\t", 
            quote = F, col.names = F, row.names = F)

degree <- degree(g)
cen <- data.frame(vertex.name=V(g)$name, degree=degree(g), closeness=closeness(g), betweenness=betweenness(g))
write.table(cen,"./res/cen.txt", sep = "\t", quote = F, col.names = T, row.names = F)
# 
# vv <- c("???????????? ??????","CALIS","?????????","?????? ?????????","???????????????","??????","?????????","?????? ?????????","?????????","????????? ??????",
#         "????????? ?????????","????????? ?????????","????????? ??????","????????? ??????","????????????","??????","??????????????????","?????? ?????????","?????????", 
#         "?????? ?????????","Wisdom Library","????????? ?????????","Interlibrary Loan","?????? ??????","????????????","MOOC","???????????????","?????? ?????????",
#         "????????????","??????","?????????","?????? ??????","????????? ????????????","?????????","????????? ??????","??????","????????? ?????????","??? ?????????","????????? ??????",
#         "????????? ?????????","???????????? ?????????","????????????","????????? ??????","?????? ?????? ?????? ??????","????????? ??????","?????? ??????","????????? ??????","?????????",
#         "?????????","????????????","????????????","??????","?????? ?????? ?????????","????????????","???????????????")

node <- read.delim("res/ttt.txt", check.names = F)
arrange(node,desc(`Degree Centrality`))
