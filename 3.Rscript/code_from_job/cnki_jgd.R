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
cnkifilename <- dir()[str_detect(dir(),"CNKI-")]

# notefirst format
# 为排除同名现象，加上单位
# 作者与单位无法匹配，会存在作者3个单位2个的情况
# cnki中，不同文献类型节点标签是不一样的
# 作者分隔符有的是","


setClass("cnkiAB", representation(ti = "character", year = "character", type = "character", 
                                  au = "list", kw = "list", organization = "list"))
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
                   au = AuList, organization = OrgList,  kw = KwList)
    return(profile)
  }else return("XML Document Error")
}


cnkiAB <- cnki.parser(cnkifilename)


table(cnkiAB@type)
table(cnkiAB@year)

any(cnkiAB@ti=="NO RES")
any(cnkiAB@type=="NO RES")
any(cnkiAB@au=="NO RES")# 标题,文献类型,作者节点标签是一致的

table(cnkiAB@type,cnkiAB@year)
table(cnkiAB@type[cnkiAB@organization=="NO RES"])
cnkiAB@ti[cnkiAB@type=="Newspaper"]
which(sapply(cnkiAB@au, length)-sapply(cnkiAB@organization, length)!=0)
cnkiAB@au[6:7]
cnkiAB@organization[6:7]
### thsis，newspaper不同类型的文献需要不同的元素标签

# 将文献类型为Newspaper的筛除
# 将无作者无关键词的文献剔除
index <- which(cnkiAB@au!="NO RES" & cnkiAB@kw!="NO RES" & cnkiAB@type!="Newspaper")

cnki <- new("cnkiAB", ti = cnkiAB@ti[index], year = cnkiAB@year[index], 
            type = cnkiAB@type[index], au = cnkiAB@au[index], 
            organization = cnkiAB@organization[index],  kw = cnkiAB@kw[index])

# 频次表及可视图
# 年代
d1 <- as.tbl(as.data.frame(table(cnki@year)))

p1 <-  d1 %>% 
  plot_ly(x = ~Var1, y = ~Freq, type = "bar", text = ~Freq, textposition = 'outside', textfont = list(size=12)) %>% 
  layout(xaxis = list(title = "年份", tickangle = -45, titlefont = list(size=18), tickfont = list(size=10)),
         yaxis = list(title = "发表文献(篇)", titlefont = list(size=18)), 
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

# 高频词
d5 <- as.tbl(as.data.frame(table(unlist(cnki@kw)), stringsAsFactors = F)) %>% 
  arrange(desc(Freq)) %>% 
  dplyr::rename(Keyword=Var1)

p2 <- as.data.frame(table(table(unlist(cnki@kw)))) %>% 
  dplyr::mutate(cumfreq=(cumsum(Freq)/sum(Freq))*100) %>% 
  plot_ly(x=~Var1, y=~cumfreq, type = 'scatter', mode = 'lines+markers') %>% 
  layout(xaxis = list(title = "关键词频次", tickangle = -45),
         yaxis = list(title = "累计频率(100%)"), 
         margin = list(b = 100), 
         showlegend = FALSE)  %>% 
  add_trace()
  plot_ly(x=5, y=~cumfreq, type = 'lines')
  
print(p2)

d5_cum <- as.data.frame(table(table(unlist(cnki@kw)))) %>% 
  dplyr::mutate(cumfreq=(cumsum(Freq)/sum(Freq))*100)

write.table(d5_cum, "./res/高频词累积.txt", row.names = F, quote = F, sep = "\t")

d5_height <- d5[d5$Freq>10,]
minfreq <- 10

p5 <- wordcloud2::wordcloud2(d5_height)
print(p5)
write.table(d5, "./res/keyword_freq.txt", row.names = F, quote = F, sep = "\t")
write.table(d5_height, "./res/高频词.txt", row.names = F, quote = F, sep = "\t")
# 矩阵
term <- d5$Keyword
term.list <- cnki@kw
names(term.list) <- 1:length(term.list)

if(minfreq != 1){
  df <- as.tbl(as.data.frame(table(unlist(term.list)), stringsAsFactors = F))
  term <- df$Var1[df$Freq >= minfreq]
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


