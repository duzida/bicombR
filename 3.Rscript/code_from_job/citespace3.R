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
path <- "G:/study/1.bicombR/6.example"
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
library(XML)

### 1.1.3 set global options
options(stringsAsFactors = FALSE)
options(encoding="utf-8")
# Sys.setlocale("LC_ALL","Chinese") 

### 1.1.4 检索下载完文献后，将query里的数据复制并重命名,可以根据pattern参数选定数据源样式
file.copy(dir("1.query/cssci_1804/", pattern = "LY.*.txt", full.names = T), "2.input/")

setwd("2.input/")
filename <- dir("./")
cssci <- unlist(lapply(filename, readLines))
file.rename(filename, paste0("download_", 1:length(filename), ".txt"))
setwd("../")

## 1.2 字段抽取
#### 删除不止一个匹配项时，需要用str_remove_all，而不是str_remove
#### fund 基金字段也是一篇文献对应多个基金，但是由于没有分析必要，因此只将其格式设置为character，而非list

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
  ky <- tolower(ky)
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
  refer <- sapply(refer,  function(x)str_remove_all(x,"^\\d+\\."))
  
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

#### 机构和作者是否一一对应
#### 如果作者和机构不对应的话，将机构list的第一个机构按照作者list长度重复

if(!identical(sapply(docAB2@author, length), sapply(docAB2@organization, length))){
  tmp <- which(sapply(docAB2@author, length) != sapply(docAB2@organization, length))
  for(i in 1:length(tmp)){
    docAB2@organization[[tmp[i]]] <- rep(docAB2@organization[[tmp[i]]][1], length(docAB2@author[[tmp[i]]]))
  }
}

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

cssci2 <- cssci[-tmp2]
ifelse(length(cssci2[str_detect(cssci2, "【来源篇名】")]) == length(unique(cssci2[str_detect(cssci2, "【来源篇名】")]))
,writeLines(cssci2, "./1.query/cssci_1804/alldata.txt"), print("still have duplicate title"))


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
# 不能直接将input文件夹中的文件导入citespace进行分析，应该是预处理后的数据再做wos转换
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
  tmp1 <- unlist(docAB2@author)
  tmp2 <- unlist(docAB2@organization)
  names(sort(table(tmp2[tmp1 %in% x]), decreasing = T)[1])
})

author1_d$organization <- sapply(author1_d$author, function(x){
  tmp <- sapply(docAB2@author, function(y)y[[1]])
  docAB2@organization[[match(x, tmp)]][1]
})


author_d <- data.frame(author=unlist(docAB2@author), organization=unlist(docAB2@organization))%>% 
  dplyr::group_by(author, organization)%>% 
  dplyr::mutate(freq = n())%>% 
  arrange(desc(freq))%>% 
  ungroup()%>%
  unique()

author$author <- as.character(author1$author1)
author$organization <- as.character(author1$organization)

author1 <- data.frame(author1=sapply(docAB2@author, function(y)y[[1]]), organization=sapply(docAB2@organization, function(y)y[[1]]))%>% 
  dplyr::group_by(author1, organization)%>% 
  dplyr::mutate(freq = n())%>% 
  arrange(desc(freq))%>% 
  ungroup()%>%
  unique()

author1$author1 <- as.character(author1$author1)
author1$organization <- as.character(author1$organization)


organization_d <- as.tbl(as.data.frame(table(unlist(docAB2@organization)), stringsAsFactors = FALSE))%>% 
  arrange(desc(Freq))%>% 
  rename(organization=Var1)

write.table(author_d, "./6.res/author/author_d.txt", quote = F, col.names = NA, sep = "\t")
write.table(author1_d, "./6.res/author/author1_d.txt", quote = F, col.names = NA, sep = "\t")
write.table(organization_d, "./6.res/author/organization_d.txt", quote = F, col.names = NA, sep = "\t")

### 1.4.3 reference
reference_d <- as.tbl(as.data.frame(table(sapply(docAB2@reference, function(x)x[1])), stringsAsFactors = FALSE))%>% 
  arrange(desc(Freq))%>% 
  rename(reference=Var1)

reference_d$reference <- sapply(reference_d$reference, function(x){
  if(str_detect(x, "(\\w+\\.)\\..*")){
    tmp1 <- str_replace(x, "(\\w+\\.)\\..*", "\\1")
    tmp2 <- str_replace_all(tmp1, "\\.", "_")
    paste0(tmp2, str_remove(x, tmp1))
  }else(x)
})

reference_d <- tidyr::separate(reference_d, col = 1, sep = "\\.", into = c("author","title", "journal", "year", "Volume_issue"))
reference_d <- tidyr::separate(reference_d, col = "Volume_issue", sep = ":", into = c("Volume_issue","page"))

reference_d$Volume_issue <- str_remove_all(reference_d$Volume_issue, "\\)$|）$")
reference_d$Volume_issue <- str_replace_all(reference_d$Volume_issue, "（", "(")
reference_d <- tidyr::separate(reference_d, col = "Volume_issue", sep = "\\(", into = c("Volume","issue"))
reference_d$issue <- str_split(reference_d$Volume_issue, "\\(")

write.table(reference_d, "./6.res/journal/reference_d.txt", quote = F, col.names = NA, sep = "\t")

### 1.4.4 keyword



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


#### 关键词同义词合并
docAB2@keyword <- sapply(docAB2@keyword, function(x){
  # x <- str_replace_all(x, "科研成果转化", "科技成果转化")
  # x <- str_replace_all(x, "产学研结合", "产学研合作")
  # x <- str_replace_all(x, "科技创新", "技术创新")
  # x <- str_replace_all(x, "大学|高等学校|高等院校", "高校")
  ### 匹配双字节字符[^x00-xff];匹配中文字符[u4e00-u9fa5] 
  x <- str_replace_all(x, "citespace.*", "citespace")})
  
docAB2@keyword <- sapply(docAB2@keyword, function(x){
  x <- str_replace_all(x, "科研成果转化", "科技成果转化")
  x <- str_replace_all(x, "产学研结合", "产学研合作")
  x <- str_replace_all(x, "科技创新", "技术创新")
  x <- str_replace_all(x, "大学|高等学校|高等院校", "高校")
  x <- str_replace_all(x, "高校技术转移", "大学技术转移")

  x <- unique(x)
})

keyword_d <- as.tbl(as.data.frame(table(unlist(docAB2@keyword))))%>% 

  arrange(desc(Freq))%>% 
  rename(keyword=Var1)

keyword_d$keyword <- as.character(keyword_d$keyword)

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


### 1.4.5 描述性数据
#### 高产作者
au_threshold <- 0.749*(author1_d$Freq[1]^0.5)
core_author <- filter(author1_d, Freq >= au_threshold)

au_info <- data.frame(article_num = sum(author1_d$Freq), author_num = nrow(author_d), author1_num = nrow(author1_d), 
                      au_threshold, core_author_num = nrow(core_author), core_author_sum = sum(core_author$Freq), 
                      core_author_aver = mean(core_author$Freq))

au_info <- dplyr::mutate(au_info, au_cooperation = author_num/article_num)

## 1.5 数据分析
### 1.5.1出现年份区间

time_zone <- function(y, field, obj){
  field <- match.arg(field, slotNames(obj))
  
  res <- sort(unique(obj@year[sapply(slot(obj, field), function(x){
    any(str_detect(x,y))
  })]))
  
  return(res)
}

time_zone(y = "citespace", obj = docAB2, field = "keyword")
sapply(as.character(keyword_d$keyword[1:10]), function(x) time_zone(y = x, obj = docAB2, field = "keyword"))

?tidyr::separate_rows()

### 突现词
burstindex <- function(term, field, obj, timewindow){
  field <- match.arg(field, slotNames(obj)[8:11])
  
  tmp1 <- slot(obj, field)
  
  if(!(term %in% unique(unlist(tmp1)))){
    stop("term error")
  }
  
  n1 <- length(which(unlist(tmp1[obj@year==timewindow]) %in% term))+0.01
  n2 <- length(which(unlist(tmp1[obj@year<timewindow]) %in% term))+0.01
  
  Nterm <- names(sort(table(unlist(tmp1)), decreasing = T)[1])
  N1 <- length(which(unlist(tmp1[obj@year==timewindow]) %in% Nterm))+0.01
  N2 <- length(which(unlist(tmp1[obj@year<timewindow]) %in% Nterm))+0.01
  
  burst <- log(n1)*((n1/N1)/(n2/N2))
  if(burst <= 0){
    burst = 0
  }
  return(round(burst,4))
  
}

burstindex(term = "知识图谱", obj = docAB2, timewindow = 2006, field = "keyword")
sapply(as.character(keyword_d$keyword[1:30]), function(x) burstindex(term = x, obj = docAB2, timewindow = 2018, field = "keyword"))
sapply(1998:2018, function(x)burstindex(term = "知识图谱", obj = docAB2, timewindow = x, field = "keyword"))

keyword_d2 <- data.frame(keyword=sapply(docAB2@keyword, function(x) paste0(x, collapse = ",")), 
                         year=docAB2@year,stringsAsFactors = F)%>% 
  tidyr::separate_rows(keyword, sep=",")%>% 
  dplyr::group_by(keyword, year)%>% 
  dplyr::mutate(freq = n())%>% 
  ungroup()%>%
  unique()%>% 
  group_by(keyword)%>%
  dplyr::mutate(sumfreq = sum(freq))%>% 
  arrange(desc(sumfreq),year)%>% 
  ungroup()
  # slice(1:20)%>% 
  # dplyr::mutate(burst = burstindex(term = keyword, obj = docAB2, timewindow = year, field = "keyword"))
  



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


#####################
# abstract(abs) from
# 1.CNKI
#####################
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
  
  KwList <- sapply(KwList, function(x)str_remove_all(x, " "))
  
  TEST = sd(c(length(Ti), length(Year), length(AuList), length(OrgList), length(KwList)))
  
  if(TEST == 0){
    profile <- new("cnkiAB", ti = Ti, year = Year, type = Type, 
                   au = AuList, organization = OrgList,  kw = KwList, publisher = Publisher)
    return(profile)
  }else return("XML Document Error")
}

cnki <- xmlTreeParse("1.query/cnki_4473/CNKI-636942361833293750.txt", useInternalNodes = T, encoding = "UTF-8")
cnkiAB <- cnki.parser(cnkifilename)

setClass("ABprofile", representation(title = "character", author = "list",  organization = "list", ab = "character", 
                                     year = "character", journal = "character", ptype = "character", keyword = "list", 
                                     mh = "list", sh = "list", majr = "list", pmid = "character", reference = "list", 
                                     fund = "character", fund_type = "list"))

### 
