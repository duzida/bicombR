#####################
# abstract(abs) from
# 1.CSSCI
#####################

## 1.1 数据准备
### 1.1.1 Create directory
#### query：下载的原始数据和检索式一般放在query文件夹上
#### input：从cssci中下载的数据，并将文件名以download_1.txt形式命名
#### output: 在citespace中将cssci格式的数据转换成wos格式数据，保存到output文件夹
#### process: 在citespace中去重后的数据，并将文件名以download1998u55pt1.txt形式命名
#### project：在citespace中新建的项目存入此文件夹内
#### res：存放结果数据，包含作者、关键词、发文年代、期刊四个子文件夹
######## 未解决问题：怎么通过网络爬取批量下载cssci数据 ########

#### 设定主工作目录的路径path,代码就放在这个文件夹下
path <- "G:\\1.bicombR/6.example"
setwd(path)

dir.create("./1.query", showWarnings = FALSE, recursive = T)
dir.create("./2.input", showWarnings = FALSE, recursive = T)
dir.create("./3.output", showWarnings = FALSE, recursive = T)
dir.create("./4.process", showWarnings = FALSE, recursive = T)
dir.create("./5.project", showWarnings = FALSE, recursive = T)
dir.create("./6.res/author", showWarnings = FALSE, recursive = T)
dir.create("./6.res/year", showWarnings = FALSE, recursive = T)
dir.create("./6.res/keyword", showWarnings = FALSE, recursive = T)
dir.create("./6.res/journal", showWarnings = FALSE, recursive = T)

### 1.1.2 Load libraries
library(Matrix)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(tidyr)

### 1.1.3 set global options
options(stringsAsFactors = FALSE)
options(encoding="utf-8")
# Sys.setlocale("LC_ALL","Chinese") 

### 1.1.4 检索下载完文献后，将query里的数据复制并重命名
file.copy(dir("1.query/cssci_1804/", pattern = ".txt", full.names = T), "2.input/")

setwd("2.input/")
filename <- dir("./")
cssci <- unlist(lapply(filename, readLines))
file.rename(filename, paste0("download_", 1:length(filename), ".txt"))
setwd("../")

## 1.2 字段抽取
#### 删除不止一个匹配项时，需要用str_remove_all，而不是str_remove
#### fund 基金字段也是一篇文献对应多个基金，但是由于没有分析必要，因此

setClass("ABprofile", representation(title = "character", author = "list",  organization = "list", ab = "character", 
                                 year = "character", journal = "character", ptype = "character", keyword = "list", 
                                 mh = "list", sh = "list", majr = "list", pmid = "character", reference = "list", 
                                 fund = "character", fund_type = "list"))

cssci.parser <- function(cssci){
  ti <- cssci[str_detect(cssci, "【来源篇名】")]
  ti <- str_remove(ti, "【.*】")
  
  au <- cssci[str_detect(cssci, "【来源作者】")]
  au <- str_remove(au, "【来源作者】")
  au <- str_split(au, "/")
  
  organ <- cssci[str_detect(cssci, "【机构名称】")]
  organ <- str_remove(organ, "【机构名称】")
  organ <- str_split(organ, "/")
  organ <- sapply(organ, function(x)str_replace(x, "\\[.*\\](.*)\\..*$", "\\1"))

  
  yr <- cssci[str_detect(cssci, "【年代卷期】")]
  yr <- str_replace_all(yr, "【年代卷期】(\\d+),.*$", "\\1")
  
  jl <- cssci[str_detect(cssci, "【期    刊】")]
  jl <- str_remove(jl, "【期    刊】")

  
  ky <- cssci[str_detect(cssci, "【关 键 词】")]
  ky <- str_remove_all(ky, "^【关 键 词】")
  ky <- str_split(ky, "/")
  
  r1 <- which(str_detect(cssci, "【参考文献】"))
  r2 <- which(str_detect(cssci, "-----------------------------------------------------------------------"))
  refer <- list()
  if(length(r1)==length(r2)){
    for(i in 1:length(r1)){
      ifelse(r1[i]+1==r2[i], refer[[i]]<-"", refer[[i]] <- cssci[(r1[i]+1):(r2[i]-1)])
    }
  }else{
    refer <- NULL
    warning("CSSCi Reference Data error!")
  }
  
  fd <- cssci[str_detect(cssci, "【基    金】")]
  
  fund_tp <- cssci[str_detect(cssci, "【基金类别】")]
  fund_tp <- str_remove_all(fund_tp, "【基金类别】|/$")
  fund_tp <- str_split(fund_tp, "/")
  
  
  TEST = sd(c(length(ti), length(au), length(organ), length(yr), length(jl), 
              length(ky), length(refer), length(fd), length(fund_tp)))
  
  
  # ab = "character", ptype = "character", mh = "list", sh = "list", majr = "list", pmid = "character",
  # 这些字段都没有，不需要管
  
  
  if(TEST == 0){
    profile <- new("ABprofile", title = ti, author = au,  organization = organ, year = yr, journal = jl, 
                   keyword = ky, reference = refer, fund = fd, fund_type = fund_tp)
    return(profile)
  }else return("CSSCI Document Error")
}
  
docAB <- cssci.parser(cssci = cssci)
docAB@title[1:5]
#### 机构和作者是一一对应
identical(sapply(docAB@author,length), sapply(docAB@organization,length))

which(sapply(docAB@author,length) != sapply(docAB@organization,length))

## 1.3 数据清洗
### 将文献类型为Newspaper、会议论文的筛除
### 将无作者无关键词的文献剔除
### 将标题重复文献剔除掉
index <- which(duplicated(docAB@title) | docAB@author=="" | docAB@keyword=="")

docAB2 <- new("ABprofile", title = docAB@title[-index], author = docAB@author[-index],  organization = docAB@organization[-index], 
              year = docAB@year[-index], journal = docAB@journal[-index], keyword = docAB@keyword[-index], 
              reference = docAB@reference[-index], fund = docAB@fund[-index], fund_type = docAB@fund_type[-index], 
              ab = docAB@ab[-index], mh = docAB@mh[-index], ptype = docAB@ptype[-index], pmid = docAB@pmid[-index], 
              sh = docAB@sh[-index], majr = docAB@majr[-index])


### 将去重后的原始文摘数据保存
dupti <- docAB@title[index]
dupindex <- sapply(paste0("【来源篇名】", dupti), function(x)which(str_detect(cssci, x)))
dupindex <- unlist(sapply(dupindex, function(x){
  ifelse(length(x)==1, x, x[-1])
}))

tmp <- c()
tmp2 <- c()
for(i in 1:length(dupindex)){
  tmp[i] <- match("-----------------------------------------------------------------------", cssci[dupindex[i]:length(cssci)])
  # cssci2 <- cssci2[-(dupindex[i]: (dupindex[i]+tmp-1))]
  tmp2 <- c(tmp2, (dupindex[i]: (dupindex[i]+tmp[i]-1)))
}

length(cssci2[str_detect(cssci2, "【来源篇名】")]) == length(unique(cssci2[str_detect(cssci2, "【来源篇名】")]))

cssci2 <- cssci[-tmp2]
writeLines(cssci2, "./1.query/cssci_1804/alldata.txt")

#### 更蠢的办法
# ti <- cssci[str_detect(cssci, "【来源篇名】")]
# ti2 <- unique(ti)
# 
# duplicated(ti)
# 
# dupti <- ti[-match(ti2, ti)]
# 
# dupindex <- match(dupti, cssci)
# 
# tmp <- c()
# tmp2 <- c()
# for(i in 1:length(dupindex)){
#   tmp[i] <- match("-----------------------------------------------------------------------", cssci[dupindex[i]:107431])
#   # cssci2 <- cssci2[-(dupindex[i]: (dupindex[i]+tmp-1))]
#   tmp2 <- c(tmp2, (dupindex[i]: (dupindex[i]+tmp[i]-1)))
# }
# 
# 
# cssci2 <- cssci[-tmp2]
# length(cssci2[str_detect(cssci2, "【来源篇名】")])
# length(unique(cssci2[str_detect(cssci2, "【来源篇名】")]))
# 
# ti2 <- cssci2[str_detect(cssci2, "【来源篇名】")]
# ti22 <- unique(ti2)
# 
# dupti <- ti2[-match(ti22, ti2)]
# 
# dupindex <- match(dupti, cssci2)
# 
# tmp <- c()
# tmp2 <- c()
# for(i in 1:length(dupindex)){
#   tmp[i] <- match("-----------------------------------------------------------------------", cssci2[dupindex[i]:104386])
#   # cssci2 <- cssci2[-(dupindex[i]: (dupindex[i]+tmp-1))]
#   tmp2 <- c(tmp2, (dupindex[i]: (dupindex[i]+tmp[i]-1)))
# }
# 
# cssci2 <- cssci2[-tmp2]
# 
# # unique 4702
# rm(tmp,tmp2,ti2,ti22,dupti,dupindex)
# writeLines(cssci2, "alldata.txt")


###############################
# 发现download_3.txt在citespace中转换发生大规模丢失
# 从第二届中国科学文献计量与评价研究学术研讨会在京举行后就全部丢失
###############################

## 1.4 数据统计
### 1.4.1 year
#### 用citespace中去重后得出的年代分布数据绘制年代分布图
year_d <- readClipboard()#将年代数据复制
year_d <- plyr::ldply(str_split(year_d, "\t"))
colnames(year_d) <-  c("year", "count")
year_d$year <- as.factor(year_d$year)

#### 用抽取出的数据
year_d <- as.data.frame(table(docAB2@year))%>% 
  arrange(Var1)%>% 
  rename(year=Var1, count=Freq)

write.table(year_d,"./6.res/year/year_d.txt", quote = F, col.names = NA, sep = "\t") 

ggplot(year_d, aes(x=year, y=count, group=1))+ geom_point(color="#56B4E9",size=5)+ 
  geom_line()+ xlab("年份/年")+ ylab("发文数量/篇")+ 
  geom_text(aes(label=count), vjust=0.5, hjust=1)+ ggsave("./6.res/year/year.png")

### 1.4.2 author/organization
author_d <- as.tbl(as.data.frame(table(unlist(docAB2@author)), stringsAsFactors = FALSE))%>% 
  arrange(desc(Freq))%>% 
  rename(author=Var1)

author1_d <- as.tbl(as.data.frame(table(sapply(docAB2@author, function(x)x[1])), stringsAsFactors = FALSE))%>% 
  arrange(desc(Freq))%>% 
  rename(author=Var1)





author_d$orgranization <- sapply(author_d$author, function(x){
  # tmp <- match(x,docAB2@author)
  # docAB2@organization[[tmp]][str_detect(docAB2@author[tmp], x)]
  index <- sapply(docAB2@author, function(y)str_detect(y, x))
  tmp <- docAB2@organization[index]
  names(sort(table(unlist(tmp)), decreasing = T)[1])
  # tmp <- as.data.frame(table(unlist(tmp)))%>% 
  #   arrange(desc(Freq)) 
  # as.character(tmp[1,1])
})



write.table(author_d, "./6.res/author/author_d.txt", quote = F, col.names = NA, sep = "\t")
write.table(author1_d, "./6.res/author/author1_d.txt", quote = F, col.names = NA, sep = "\t")



res_au_1$instruc <- sapply(res_au_1$Var1, function(x){
  tmp <- cssci_wos[str_detect(cssci_wos, paste0("^C1 \\[", x, "\\]"))]
  tmp <- str_remove(tmp, "/.*$")
  tmp <- str_remove(tmp, "^C1 \\[.*\\]")
  # tmp <- as.character(tmp)
  tmp <- as.data.frame(table(tmp))%>% 
    arrange(desc(Freq)) 
  as.character(tmp[1,1])
})
res_au_1$instruc <- str_remove(res_au_1$instruc, "\\.$")
### 1.4.3 reference


### 1.4.5 描述性数据


### 1.4.4 keyword

keyword_d <- as.tbl(as.data.frame(table(unlist(docAB2@keyword))))%>% 
  arrange(desc(Freq))%>% 
  rename(keyword=Var1)

docAB2@keyword <- sapply(docAB2@keyword, function(x){
  x <- str_replace_all(x, "科研成果转化", "科技成果转化")
  x <- str_replace_all(x, "产学研结合", "产学研合作")
  x <- str_replace_all(x, "科技创新", "技术创新")
  x <- str_replace_all(x, "大学|高等学校|高等院校", "高校")
  x <- str_replace_all(x, "高校技术转移", "大学技术转移")
  x <- unique(x)
})

keyword_d <- as.tbl(as.data.frame(table(unlist(docAB2@keyword))))%>% 
  arrange(docAB2@keywordsc(Freq))%>% 
  rename(keyword=Var1)

write.table(keyword_d, "./6.res/keyword/keyword_d.txt", quote = F, col.names = NA, sep = "\t")

as.tbl(as.data.frame(table(table(unlist(docAB2@keyword))))) %>% 
  dplyr::mutate(cumfreq=(cumsum(Freq)/sum(Freq))*100) %>% 
  plot_ly(x=~Var1, y=~cumfreq, type = 'scatter', mode = 'lines+markers') %>% 
  layout(xaxis = list(title = "keyword Frequence", tickangle = -45),
         yaxis = list(title = "Cumulative Frequency(100%)"), 
         # margin = list(b = 100), 
         showlegend = FALSE) 


minfreq <- 10
# 矩阵
de.term <- res_de$Var1[res_de$Freq >= minfreq]
de.term <- intersect(readClipboard(), res_de$Var1)

de_term.list <- sapply(de, function(x)return(intersect(x, de.term)))
de_term.list <- de_term.list[sapply(de_term.list, length)!=0]  

de_term.list[1:5]

tdm <- Matrix(0, nrow = length(de.term), ncol = length(de_term.list), dimnames = list(de.term, names(de_term.list)))
for (i in 1:length(de_term.list)){
  tdm[de_term.list[[i]], i] <- 1
}

write.table(as.matrix(tdm), "../../../result/关键词/tdm.txt", sep = "\t", quote = F, col.names = NA)


com <- Matrix(0, nrow = length(de.term), ncol = length(de.term), dimnames = list(de.term, de.term))
for (i in 1:length(de_term.list)){
  com[de_term.list[[i]], de_term.list[[i]]] <- 1 + com[de_term.list[[i]], de_term.list[[i]]]
}

com[1:5, 1:5]
write.table(as.matrix(com), "../../../result/关键词/2com.txt", sep = "\t", quote = F, col.names = NA)




#CR
#1213
cr <- cssci_wos[str_detect(cssci_wos, "^CR")]
index_cr <- which(str_detect(cssci_wos, "^CR"))

index_nr <- c()
for(j in 1:length(index_cr)){
  i <- index_cr[j]
  index_nr[j] <- which(str_detect(cssci_wos[i:60125], "^NR"))[1]+i-1
}
# index_nr <- which(str_detect(cssci_wos, "^NR"))

index_cr[1:10]
index_nr[1:10]

cr_all <- list()

for(i in 1:length(index_cr)){
  cr_all[[i]] <- cssci_wos[index_cr[i]:(index_nr[i]-1)]
}

cr_all[1:5]

cr_all <- sapply(cr_all, function(x){
  x[1] <- str_remove(x[1], "^CR ")
  # x <- str_remove(x, ", $")
  x <- str_remove(x, "^   ")
  # names(x) <- NULL
})

res_cr <- as.tbl(as.data.frame(table(unlist(cr_all))))%>% 
  arrange(desc(Freq))

library(tidyr)
res_cr$Var1 <- as.character(res_cr$Var1)
res_cr$Var1 <- str_replace(res_cr$Var1, "(^\\w+), (\\w+, \\d+,)", "\\1_\\2")
res_cr <- tidyr::separate(res_cr, col = 1, sep = ", ", into = c("author", "year", "journal", "V", "P"))
write.table(res_cr, "../../result/期刊/res_cr.txt", quote = F, col.names = NA, sep = "\t")


#Cited reference CR
#40460
# cr <- cssci_wos[str_detect(cssci_wos, "^CR")]
index_cr <- which(str_detect(cssci_wos, "^CR"))

index_nr <- c()
for(j in 1:length(index_cr)){
  i <- index_cr[j]
  index_nr[j] <- which(str_detect(cssci_wos[i:length(cssci_wos)], "^NR"))[1]+i-1
}
# index_nr <- which(str_detect(cssci_wos, "^NR"))

index_cr[1:10]
index_nr[1:10]

cr_all <- list()

for(i in 1:length(index_cr)){
  cr_all[[i]] <- cssci_wos[index_cr[i]:(index_nr[i]-1)]
}

cr_all[1:5]

cr_all <- sapply(cr_all, function(x){
  x[1] <- str_remove(x[1], "^CR ")
  # x <- str_remove(x, ", $")
  x <- str_remove(x, "^   ")
  # names(x) <- NULL
})

res_cr <- as.tbl(as.data.frame(table(unlist(cr_all))))%>% 
  arrange(desc(Freq))

library(tidyr)
res_cr$Var1 <- as.character(res_cr$Var1)
## 有的作者名字直接有逗号 会影响分隔，因此将其替换为下换线_
res_cr$Var1 <- str_replace(res_cr$Var1, "(^\\w+), (\\w+, \\d+,)", "\\1_\\2")
res_cr <- tidyr::separate(res_cr, col = 1, sep = ", ", into = c("author", "year", "journal", "V", "P"))
write.table(res_cr, "res/期刊/res_cr.txt", quote = F, col.names = NA, sep = "\t")

#总引文数
length(unlist(cr_all))

res_jr <- as.tbl(as.data.frame(table(tolower(res_cr$journal))))%>% 
  arrange(desc(Freq))
write.table(res_jr, "res/期刊/res_jr.txt", quote = F, col.names = NA, sep = "\t")

writeLines(cssci_wos, "cssci_wos.txt")

length(unlist(str_extract_all(cr_all, ", Research Policy,")))

