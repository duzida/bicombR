# Load libraries
library(Matrix)
library(dplyr)
library(stringr)

#Create Results directory
#rm(list=ls())
# gc()
# setwd("../res2/")
dir.create("./process", showWarnings = FALSE, recursive = T)

# set global options
options(stringsAsFactors = FALSE)
options(encoding="utf-8")
# Sys.setlocale("LC_ALL","Chinese") 

setwd("成果转化/")
file.copy(dir("./"), "../process/")
setwd("../技术转移/")
file.copy(dir("./"), "../process/")
setwd("../process/")

filename <- dir("./")

# setwd("process/list/")
cssci <- unlist(lapply(filename, readLines))
# setwd("../../")

ti <- cssci[str_detect(cssci, "【来源篇名】")]
ti2 <- unique(ti)

dupti <- ti[-match(ti2, ti)]

dupindex <- match(dupti, cssci)

tmp <- c()
tmp2 <- c()
for(i in 1:length(dupindex)){
  tmp[i] <- match("-----------------------------------------------------------------------", cssci[dupindex[i]:107431])
  # cssci2 <- cssci2[-(dupindex[i]: (dupindex[i]+tmp-1))]
  tmp2 <- c(tmp2, (dupindex[i]: (dupindex[i]+tmp[i]-1)))
}

cssci2 <- cssci[-tmp2]
length(cssci2[str_detect(cssci2, "【来源篇名】")])
length(unique(cssci2[str_detect(cssci2, "【来源篇名】")]))

ti2 <- cssci2[str_detect(cssci2, "【来源篇名】")]
ti22 <- unique(ti2)

dupti <- ti2[-match(ti22, ti2)]

dupindex <- match(dupti, cssci2)

tmp <- c()
tmp2 <- c()
for(i in 1:length(dupindex)){
  tmp[i] <- match("-----------------------------------------------------------------------", cssci2[dupindex[i]:104386])
  # cssci2 <- cssci2[-(dupindex[i]: (dupindex[i]+tmp-1))]
  tmp2 <- c(tmp2, (dupindex[i]: (dupindex[i]+tmp[i]-1)))
}

cssci2 <- cssci2[-tmp2]

writeLines(cssci2, "process/download_1.txt")

dir(path = "process/list/")
setwd("process/list/")
file.rename(filename, paste0("download_", 1:50, ".txt"))
setwd("../../")

#
ky <- cssci2[str_detect(cssci2, "【关 键 词】")]
ky <- str_remove_all(ky, "^【关 键 词】")
ky <- str_split(ky, "/")

ky2 <- unlist(ky)

res <- as.tbl(as.data.frame(table(ky2)))%>% 
  arrange(desc(Freq))
write.table(res, "keyword_freq.txt", quote = F, col.names = NA, sep = "\t")

setwd("E://job/citespace2800/res3/data/input")
filename <- dir()

file.rename(filename, paste0("download_", 1:11, ".txt"))

# year

# year_d <- data.frame(year=as.character(1998:2018), count=as.integer(readClipboard()))
year_d <- plyr::ldply(str_split(readClipboard(), pattern = "\\t"))
colnames(year_d) <- c("year", "count")
library(ggplot2)
year_d$year <- as.factor(year_d$year)
year_d$count <- as.numeric(year_d$count)
ggplot(year_d, aes(x=year, y=count, group=1))+ geom_point(color="#56B4E9", size=5)+ 
  geom_line()+ xlab("年份/年")+ ylab("发文数量/篇")+ 
  theme(axis.text.x=element_text(angle=45,size=10,hjust=0.8), axis.title.y=element_text(size=14), 
        axis.title.x=element_text(size=14))+ 
  geom_text(aes(label=count), vjust=0, hjust=-0.1)+ ggsave("res2/年份/year.png")  

#############################################################################
# 讲结果合并
setwd("../../../data/project/")

project <- dir("./")
project
# readLines(project[1])
cssci_wos <- unlist(lapply(project, readLines))
cssci_wos[1:30]

pt <-  cssci_wos[str_detect(cssci_wos, "^PT")]

#TI
ti <- cssci_wos[str_detect(cssci_wos, "^TI")]

# AU
au <- cssci_wos[str_detect(cssci_wos, "^AU")]
index_au <- which(str_detect(cssci_wos, "^AU"))
index_af <- which(str_detect(cssci_wos, "^AF"))

au_all <- list()

for(i in 1:length(index_au)){
  au_all[[i]] <- cssci_wos[index_au[i]:(index_af[i]-1)]
}

au_all <- sapply(au_all, function(x){
  x[1] <- str_remove(x[1], "^AU ")
  x <- str_remove(x, ", $")
  x <- str_remove(x, "^   ")
  # names(x) <- NULL
})

au_all[1:10]

au_all2 <- unlist(au_all)

res_au_all <- as.tbl(as.data.frame(table(au_all2)))%>% 
  arrange(desc(Freq))

res_au_1 <- as.tbl(as.data.frame(table(sapply(au_all, function(x)x[1]))))%>% 
         arrange(desc(Freq)) 

write.table(res_au_1, "../../result/作者/res_au_1.txt", quote = F, col.names = NA, sep = "\t")
write.table(res_au_all, "../../result/作者/res_au_all.txt", quote = F, col.names = NA, sep = "\t")

gc()

au[str_detect(au, "AU 刘希宋")]


str_detect(cssci_wos[1:10], "^AU.*AF")

#PY
py <- cssci_wos[str_detect(cssci_wos, "^PY")]

#instruction
cssci_wos[str_detect(cssci_wos, "\\[饶凯\\]")]
cssci_wos[str_detect(cssci_wos, "^C1 \\[刘希宋\\]")]
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
write.table(res_au_1, "../../result/作者/res_au_1.txt", quote = F, col.names = NA, sep = "\t")

cssci_wos[str_detect(cssci_wos, "陈劲")]

cssci_wos[3101:3150]

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

#keyword DE
cssci_wos[1:30]
de <- cssci_wos[str_detect(cssci_wos, "^DE")]
de <- str_split(str_remove(de, "^DE "), "; ")

res_de <- as.tbl(as.data.frame(table(unlist(de))))%>% 
  arrange(desc(Freq))

res_de
write.table(res_de, "../../../result/关键词/res_de.txt", quote = F, col.names = NA, sep = "\t")

de[1:5]

de <- sapply(de, function(x){
  x <- str_replace_all(x, "科研成果转化|技术成果转化", "科技成果转化")
  x <- str_replace_all(x, "科研成果", "科技成果")
  x <- str_replace_all(x, "科技创新", "技术创新")
  x <- str_replace_all(x, "大学|高等学校|高等院校", "高校")
  x <- str_replace_all(x, "高校技术转移", "大学技术转移")
})

res_de <- as.tbl(as.data.frame(table(unlist(de)), stringsAsFactors = F))%>% 
  arrange(desc(Freq))

res_de
write.table(res_de, "../../../result/关键词/res_de.txt", quote = F, col.names = NA, sep = "\t")

library(plotly)
as.tbl(as.data.frame(table(table(unlist(de))))) %>% 
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

###
ab <- cssci_wos[str_detect(cssci_wos, "^AB")]
ab[str_detect(ab, "经济增长")]
ab[str_detect(ab, "自主研发")]
ab[str_detect(ab, "风险投资")]
ab[str_detect(ab, "国际技术转移")]
ab[str_detect(ab, "技术创新")]

####CNKI
filename <- dir("./data/")

setwd("data/")
cnki <- unlist(lapply(filename, readLines))
setwd("../")

cnki[1:10]


# AU cnki作者字段都在一行
au <- cnki[str_detect(cnki, "^A1")]
index_au <- which(str_detect(cnki, "^A1"))
# index_af <- which(str_detect(cnki, "^AD"))

au_all <- cnki[index_au]
au_all[1:10]

au_all <- str_remove(au_all, "^A1 ")
au_all <- str_remove(au_all, ";$")
au_all <- str_split(au_all, ";")


res_au_all <- as.tbl(as.data.frame(table(unlist(au_all))))%>% 
  arrange(desc(Freq))

res_au_1 <- as.tbl(as.data.frame(table(sapply(au_all, function(x)x[1]))))%>% 
  arrange(desc(Freq)) 

write.table(res_au_1, "res2/作者/res_au_1.txt", quote = F, col.names = NA, sep = "\t")
write.table(res_au_all, "res2/作者/res_au_all.txt", quote = F, col.names = NA, sep = "\t")

gc()

#instruction
AD <- cnki[str_detect(cnki, "^AD")]
#YR
YR <- cnki[str_detect(cnki, "^YR")]

index_au <- which(str_detect(cnki, "^A1"))
index_ad <- which(str_detect(cnki, "^AD"))
index_yr <- which(str_detect(cnki, "^YR"))

au1 <- cnki[intersect(index_ad-1, index_au)]
ad1 <- cnki[intersect(index_ad-1, index_au)+1]

au1[1:5]
ad1[1:5]

sapply(as.character(res_au_1$Var1)[1:18], function(x){
  unique(ad1[str_detect(au1, x)])
})

au2 <- cnki[intersect(index_yr-4, index_au)]
yr2 <- cnki[intersect(index_yr, index_au+4)]

yr2 <- str_remove(yr2, "YR ")

au2[1:5]
yr2[1:5]

t(sapply(as.character(res_au_1$Var1)[1:18], function(x){
  sort(unique(yr2[str_detect(au2, x)]))[1]
}))

res_au_1$instruc <- str_detect(au1, as.character(res_au_1$Var1)[1:10])


cssci_wos[str_detect(cssci_wos, "\\[饶凯\\]")]
cssci_wos[str_detect(cssci_wos, "^C1 \\[刘希宋\\]")]
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
write.table(res_au_1, "../../result/作者/res_au_1.txt", quote = F, col.names = NA, sep = "\t")

writeLines(cnki, "alldata.txt")

k1 <- cnki[str_detect(cnki, "^K1")]
k1 <- str_remove(k1, "K1 ")
k1 <- str_remove(k1, ";$")
k1_list <- str_split(k1, ";")
k1_list[1:20]

k1_df <- as.tbl(as.data.frame(table(unlist(k1_list))))%>% 
  arrange(desc(Freq))
k1_df[1:20,]

write.table(k1_df, "res2/关键词/res_df.txt", quote = F, col.names = NA, sep = "\t")


#
table(sapply(au_all, length))
which(sapply(au_all, length)==10)
au_all[202]

# JF:journal
JF <- cnki[str_detect(cnki, "^JF")]
JF <- str_remove(JF, "JF ")
JF[1:5]

JF_df <-  as.tbl(as.data.frame(table(JF)))%>% 
  arrange(desc(Freq))
write.table(JF_df, "res2/期刊/res_df.txt", quote = F, col.names = NA, sep = "\t")

### 同义词合并
k1_list2 <- sapply(k1_list, function(x){
  x <- str_replace_all(x, "高职教育|高等职业技术教育", "高等职业教育")
  x <- str_replace_all(x, "高等职业院校", "高职院校")
  x <- str_replace_all(x, "职业教育课程", "职教课程")
  x <- str_replace_all(x, "职业技术教育", "职业教育")
  x <- unique(x)
})



res_k1 <- as.tbl(as.data.frame(table(unlist(k1_list2)), stringsAsFactors = F))%>% 
  arrange(desc(Freq))

res_k1
write.table(res_k1, "res2/关键词/res_k1.txt", quote = F, col.names = NA, sep = "\t")

library(plotly)
as.tbl(as.data.frame(table(table(unlist(k1_list2))))) %>% 
  dplyr::mutate(cumfreq=(cumsum(Freq)/sum(Freq))*100) %>% 
  plot_ly(x=~Var1, y=~cumfreq, type = 'scatter', mode = 'lines+markers') %>% 
  layout(xaxis = list(title = "keyword Frequence", tickangle = -45),
         yaxis = list(title = "Cumulative Frequency(100%)"), 
         # margin = list(b = 100), 
         showlegend = FALSE)

minfreq <- 11
# 矩阵
de.term <- res_k1$Var1[res_k1$Freq >= minfreq]
# de.term <- intersect(readClipboard(), res_k1$Var1)

de_term.list <- sapply(k1_list2, function(x)return(intersect(x, de.term)))
de_term.list <- de_term.list[sapply(de_term.list, length)!=0]  

de_term.list[1:5]

tdm <- Matrix(0, nrow = length(de.term), ncol = length(de_term.list), dimnames = list(de.term, names(de_term.list)))
for (i in 1:length(de_term.list)){
  tdm[de_term.list[[i]], i] <- 1
}

write.table(as.matrix(tdm), "res2/关键词/tdm.txt", sep = "\t", quote = F, col.names = NA)


com <- Matrix(0, nrow = length(de.term), ncol = length(de.term), dimnames = list(de.term, de.term))
for (i in 1:length(de_term.list)){
  com[de_term.list[[i]], de_term.list[[i]]] <- 1 + com[de_term.list[[i]], de_term.list[[i]]]
}

com[1:5, 1:5]
write.table(as.matrix(com), "res2/关键词/com.txt", sep = "\t", quote = F, col.names = NA)

121677

library(igraph)
com_g <- graph_from_adjacency_matrix(as.matrix(com), mode = "undirected", weighted = T, diag = F)
graph.density(com_g)

### abstract
ab <- cssci_wos[str_detect(cssci_wos, "^AB")]
ab[str_detect(ab, "经济增长")]
ab[str_detect(ab, "自主研发")]
ab[str_detect(ab, "风险投资")]
ab[str_detect(ab, "国际技术转移")]
ab[str_detect(ab, "技术创新")]
