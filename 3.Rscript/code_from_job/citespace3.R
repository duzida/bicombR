#####################
# 1. 环境准备
#####################

## 1.1 clear environment
rm(list=ls())
gc()

## 1.2 set working direcory
path <- "G:/study/1.bicombR/6.example"
setwd(path)

## 1.3 print session information
sink("info.txt")
sessionInfo()
sink()

## 1.4 create directory
### query：下载的原始数据和检索式一般放在query文件夹上
### input：从cssci中下载的数据，并将文件名以download_1.txt形式命名
### output: 在citespace中将cssci格式的数据转换成wos格式数据，保存到output文件夹
### process: 在citespace中去重后的数据，并将文件名以download1998u55pt1.txt形式命名
### project：在citespace中新建的项目存入此文件夹内
### res：存放结果数据，包含作者、关键词、发文年代、期刊四个子文件夹
######## 未解决问题：怎么通过网络爬取批量下载cssci数据 ########
######## 未解决问题：怎么通过网络爬取批量下载wos数据 ########

dir.create("./1.query", showWarnings = FALSE, recursive = T)
dir.create("./2.input", showWarnings = FALSE, recursive = T)
dir.create("./3.output", showWarnings = FALSE, recursive = T)
dir.create("./4.process", showWarnings = FALSE, recursive = T)
dir.create("./5.project", showWarnings = FALSE, recursive = T)
dir.create("./6.res/author", showWarnings = FALSE, recursive = T)
dir.create("./6.res/year", showWarnings = FALSE, recursive = T)
dir.create("./6.res/keyword", showWarnings = FALSE, recursive = T)
dir.create("./6.res/journal", showWarnings = FALSE, recursive = T)

## 1.5 load libraries
library(Matrix)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(tidyr)
library(XML)
library(reshape2)
library(readr)

## 1.6 set global options
options(stringsAsFactors = FALSE)

# options(encoding="utf-8")
# getOption("stringsAsFactors")
# Sys.setlocale("LC_ALL","Chinese")
locale <- Sys.getlocale()

#####################
# 2. File reading
#####################

########## 文件读取的问题：编码，语系，分隔文件
# 下载文献后，将query里的数据复制并重命名,可以根据pattern参数选定数据源样式
# WOS读取文件时不能设定编码为encoding==“utf-8”
# CSSCI读取文件时候必须设定编码为utf-8
# 这个地方如果数据文件的编码不是utf-8会有warnings，且数据量不对，如utf-8-boom就需要转成utf-8
# invalid input found on input connection 'download_3.txt' 这种警告一般是编码问题
# incomplete final line found on 'download_10.txt' 这种警告需要在文件末尾加一行空行
# 韩文没法读取，会报错，因此设置下语系Sys.setlocale("LC_ALL","Chinese")[错误做法]
# 不要设定编码环境变量options(encoding="utf-8")，就可以正常读取
# 经过str_split分割操作，使得每一篇文章相互独立，字段可以对应起来
# 文章内没有该字段的就是NA，字段的长度保持统一
# 在笔记本上cssci数据库也不需要设定utf-8，str_conv(csscidoc[1], "gbk")
##########
file_conv <- function(filepath, db="WOS"){
  db <- match.arg(db, c("WOS", "PUBMED", "CNKI", "CSSCI"))
  absep <- switch (db,
                   WOS = "ER",
                   PUBMED = "SO",
                   CSSCI = "-+",
                   CNKI = "DS"
  )
  filepattern <- switch (db,
                         WOS = "save.*.txt",
                         PUBMED = "pubmed.*.txt",
                         CSSCI = "LY.*.txt",
                         CNKI = "CNKI.*.txt"
  )
  
  file.copy(dir(filepath, pattern = filepattern, full.names = T), "./2.input/")
  
  setwd("2.input/")
  filename <- dir("./")
  doc <- unlist(lapply(filename, readLines))
  # if(db %in% c("CSSCI")){
  #   doc <- str_conv(doc, "utf-8")
  # }
  file.rename(filename, paste0("download_", 1:length(filename), ".txt"))
  setwd("../")
  
  writeLines(doc, "./1.query/alldata.txt")
  
  doc <- read_file("1.query/alldata.txt")
  doc <- unlist(str_split(doc, paste0("\r\n", absep)))
  doc <- doc[1:(length(doc)-1)]
  doc[1] <- str_c("\r\n",doc[1])
  
  print(paste0("The number of documents is ", length(doc)))
  return(doc)
}

csscidoc <- file_conv(filepath = "1.query/cssci_1804/", db = "CSSCI")
wosdoc <- file_conv(filepath = "G:/job/citespace5000/wos/1.query/", db = "WOS")
# cnkidoc <- file_conv(filepath = "G:/job/citespace5000/cnki/1.query/Refworks/", db = "CNKI")
cnkidoc <- file_conv(filepath = "1.query/cnki_750/Refworks/", db = "CNKI")
pubmeddoc <- file_conv(filepath = "1.query/pubmed_375/", db = "PUBMED")

save(list = ls(), file = "doc.RData")
load("doc.RData")
#####################
# 3. Data preprocess
#####################

## 3.1 abstract extract
########## 各个字段的格式和按照文章分解，利用正则
#### 删除不止一个匹配项时，需要用str_remove_all，而不是str_remove
#### cssci的fund 基金字段也是一篇文献对应多个基金，但是由于没有分析必要，因此只将其格式设置为character，而非list
#### wos的通讯作者地址字段是RP,全部作者地址字段是C1
#### wos的关键词字段是DE,ID是wos给文章补充的关键词
#### wos的来源期刊是SO
####直接指定两个字段是错误的，因为有时候下一个字段不固定，如DE字段后面不一定肯定就是ID字段,
#### 因此需要先找到第一个字段后面跟的字段是什么
#### 正则很强大，不需要知道下一个字段是什么
#### cnki
#### 为排除同名现象，加上单位
#### 作者与单位无法匹配，会存在作者3个单位2个的情况
#### cnki中，不同文献类型节点标签是不一样的
#### 作者分隔符有的是","
#### cnki在关键词有的是英文，大小写得统一，空格不能去除
#### cnki字段内容没有时，字段标题还有
#### pubmed
#### pubmed的medline格式AD,AU，MH还有MAJR都分开不同行的
#### 一行数据AU:\r\nAU.*
#### 多行数局AD:\r\nAD\\s+- [\\s\\S]*?\r\n\\w
#### 空字段如何处理：表现形式有""、list()、character(0)、NA、NULL：统一用NA表示
#### str_extract 匹配不到数据返回NA
#### str_extract_all 匹配不到数据返回list形式的character(0)
#### str_split 返回list对象，str_split(NA，sep)返回NA；
#### str_split(character(0)，sep)返回list()
###
##########
# setClass("ABprofile", representation(TI = "character", author = "list",  AD = "list", ab = "character",
# PDAT = "character", journal = "character", ptype = "character", keyword = "list",
# mh = "list", sh = "list", majr = "list", pmid = "character", reference = "list",
# fund = "character", fund_type = "list"))

setClass("ABprofile",
         slots = list(TI = "character", AU = "list",  AD = "list", PDAT = "character", JL = "character"))
setClass("AB_WOS", contains="ABprofile",
         slots = list(AB = "character", PT = "character", CR = "list", KW = "list"))
setClass("AB_CSSCI", contains="ABprofile",
         slots = list(Fund = "character", Fund_type = "list", CR = "list", KW = "list"))
setClass("AB_PUBMED", contains="ABprofile",
         slots = list(AB = "character", PT = "character", MH = "list", SH = "list", SMH = "list", MAJR = "list", PMID = "character"))
setClass("AB_CNKI", contains="ABprofile",
         slots = list(AB = "character", PT = "character", KW = "list"))

# setClass("db", slots = list(type = "character"))
# setGeneric("AB_parse", function(obj,...) standardGeneric("AB_parse"))
# setMethod("AB_parse", signature(object = "character"), function(obj,db,...){
#   print("da")
# })

field_extract <- function(file, field){
  Pattern <- paste0("\r\n", field, "\\s+([\\s\\S]*?)\r\n(\\w+)")
  tmp <- str_replace(str_extract(file, Pattern), Pattern, "\\1")
  # Pattern <- paste0("\r\n", field1, " ([\\s\\S]*?)\r\n", field2)
  ## tmp <- str_replace_all(unlist(str_extract_all(file, Pattern)), Pattern, "\\1")
  # tmp <- str_replace_all(str_extract(file, Pattern), Pattern, "\\1")
  # tmp <- str_replace_all(tmp, "\r\n  ", "")
  return(tmp)
}

wos.parser <- function(wosfile){
  
  ti <- field_extract(wosfile, "TI")
  ti <- str_replace_all(ti, "\r\n\\s+", " ")
  ti[ti==""] <- NA
  
  au <- field_extract(wosfile, "AU")
  au[au==""] <- NA
  au <- str_split(au, "\r\n\\s+")
  
  # organ <- str_remove(str_extract(wosfile, "\r\nRP.*"), "\r\nRP\\s+.*reprint author\\), ")
  # RP是通讯作者
  organ <- field_extract(wosfile, "C1")
  organ[organ==""] <- NA
  organ <- str_split(organ, "\r\n\\s+")
  
  yr <- str_remove(str_extract(wosfile, "\r\nPY .*"), "\r\nPY\\s+")
  yr[yr==""] <- NA
  # table(au, useNA = "always")
  # any(is.na(au))
  
  jl <- str_remove(str_extract(wosfile, "\r\nSO.*"), "\r\nSO\\s+")
  jl[jl==""] <- NA
  
  ky <- field_extract(wosfile, "DE")
  ky <- str_to_lower(str_replace_all(ky, "\r\n\\s+", " "))
  ky[ky==""] <- NA
  ky <- str_split(ky, "; ")
  # ky[1:10]
  
  refer <- field_extract(wosfile, "CR")
  refer[refer==""] <- NA
  refer <- str_split(refer, "\r\n\\s+")
  # refer[1:4]
  
  ab <- field_extract(wosfile, "AB")
  ab[ab==""] <- NA
  # ab[1:3]
  
  pt <- field_extract(wosfile, "PT")
  pt[pt==""] <- NA
  pt <- str_replace(pt, "J", "Journal Article")
  
  TEST = sd(c(length(ti), length(au), length(organ), length(yr), length(jl),
              length(ky), length(refer), length(pt), length(ab)))
  
  
  # fd,fund_tp, mh = "list", sh = "list", majr = "list", pmid = "character",
  # 这些字段都没有，不需要管
  
  if(TEST == 0){
    profile <- new("AB_WOS", TI = ti, AU = au,  AD = organ, PDAT = yr, JL = jl,
                   KW = ky, CR = refer, AB = ab, PT = pt)
    return(profile)
  }else return("WOS Document Error")
}
cnki.parser <- function(cnkifile){
  
  ti <- field_extract(cnkifile, "T1")
  ti[ti==""] <- NA
  
  au <- field_extract(cnkifile, "A1")
  au <- str_remove(au, ";$")
  au[au==""] <- NA
  au <- str_split(au, ";")
  
  organ <- field_extract(cnkifile, "AD")
  organ <- str_remove(organ, ";$")
  organ[organ==""] <- NA
  organ <- str_split(organ, ";")
  
  yr <- str_remove(str_extract(cnkifile, "\r\nYR .*"), "\r\nYR\\s+")
  yr[yr==""] <- NA
  # table(au, useNA = "always")
  # any(is.na(au))
  
  jl <- str_remove(str_extract(cnkifile, "\r\nJF.*"), "\r\nJF\\s+")
  jl[jl==""] <- NA
  
  # ky <- str_replace(str_extract(cnkifile, "\r\nK1.*"), "\r\nK1\\s+(.*[^;]);?$", "\\1")
  ky <- str_remove_all(str_extract(cnkifile, "\r\nK1.*"), "\r\nK1\\s+")
  ky <- str_remove_all(ky,"[“”’‘]|;$")
  ky <- str_replace_all(ky, ",", ";")
  ky <- str_to_lower(ky)
  ky[ky==""] <- NA
  ky <- str_split(ky, ";")
  
  ab <- field_extract(cnkifile, "AB")
  ab[ab==""] <- NA
  
  pt <- field_extract(cnkifile, "RT")
  pt[pt==""] <- NA
  
  TEST = sd(c(length(ti), length(au), length(organ), length(yr), length(jl),
              length(ky), length(pt), length(ab)))
  
  if(TEST == 0){
    profile <- new("AB_CNKI", TI = ti, AU = au,  AD = organ, PDAT = yr, JL = jl,
                   KW = ky, AB = ab, PT = pt)
    return(profile)
  }else return("CNKI Document Error")
}
cssci.parser <- function(csscifile){
  
  ti <- str_remove_all(str_extract(csscifile, "【来源篇名】[\\s\\S]*?\r\n"), "【来源篇名】|\r\n$")
  ti[ti==""] <- NA
  
  au <- str_remove_all(str_extract(csscifile, "【来源作者】[\\s\\S]*?\r\n"), "【来源作者】|\r\n$")
  au[au==""] <- NA
  au <- str_split(au, "/")
  
  organ <- str_remove_all(str_extract(csscifile, "【机构名称】[\\s\\S]*?\r\n"), "【机构名称】|\r\n$|\\[\\w*\\]|\\.\\w*")
  organ[organ==""] <- NA
  organ <- str_split(organ, "/")
  
  yr <- str_remove_all(str_extract(csscifile, "【年代卷期】[\\s\\S]*?\r\n"), "【年代卷期】|,.*\r\n$")
  yr[yr==""] <- NA
  
  jl <- str_remove_all(str_extract(csscifile, "【期    刊】[\\s\\S]*?\r\n"), "【期    刊】|\r\n$")
  jl[jl==""] <- NA
  
  ky <- str_remove_all(str_extract(csscifile, "【关 键 词】[\\s\\S]*?\r\n"), "【关 键 词】|\r\n$")
  ky[ky==""] <- NA
  ky <- str_to_lower(ky)
  ky <- str_split(ky, "/")
  
  refer <- str_remove_all(str_extract(csscifile, "【参考文献】[\\s\\S]*"), "【参考文献】\r\n")
  refer[refer==""] <- NA
  refer <- str_split(refer, "\r\n")
  refer <- sapply(refer, function(x)str_remove(x,"^\\d+\\."))
  # r1 <- which(str_detect(cssci, "【参考文献】"))
  # r2 <- which(str_detect(cssci, "-----------------------------------------------------------------------"))
  # refer <- list()
  # if(length(r1)==length(r2)){
  #   for(i in 1:length(r1)){
  #     ifelse(r1[i]+1==r2[i], refer[[i]]<-"", refer[[i]] <- cssci[(r1[i]+1):(r2[i]-1)])
  #   }
  # }else{
  #   refer <- NULL
  #   warning("CSSCi Reference Data error!")
  # }
  # refer <- sapply(refer,  function(x)str_remove_all(x,"^\\d+\\."))
  
  fd <- str_remove_all(str_extract(csscifile, "【基    金】[\\s\\S]*?\r\n"), "【基    金】|\r\n$")
  fd[fd==""] <- NA
  
  fund_tp <- str_remove_all(str_extract(csscifile, "【基金类别】[\\s\\S]*?\r\n"), "【基金类别】|/\r\n$|\r\n")
  fund_tp[fund_tp==""] <- NA
  fund_tp <- str_split(fund_tp, "/")
  
  TEST = sd(c(length(ti), length(au), length(organ), length(yr), length(jl),
              length(ky), length(refer), length(fd), length(fund_tp)))
  
  # ab = "character", ptype = "character", mh = "list", sh = "list", majr = "list", pmid = "character",
  # 这些字段都没有，不需要管
  
  if(TEST == 0){
    profile <- new("AB_CSSCI", TI = ti, AU = au,  AD = organ, PDAT = yr, JL = jl,
                   KW = ky, CR = refer, Fund = fd, Fund_type = fund_tp)
    return(profile)
  }else return("CSSCI Document Error")
}
pubmed.parser <- function(pubmedfile){
  ti <- field_extract(pubmedfile, "TI")
  ti <- str_replace_all(ti, "\r\n\\s+", " ")
  ti <- str_remove_all(ti,"^-\\s+")
  ti[ti==""] <- NA
  
  au <- str_extract_all(pubmedfile, "\r\nAU\\s+.*")
  au <- sapply(au, function(x)str_remove_all(x,"\r\nAU\\s+-\\s+|\r\n"))
  au <- lapply(au, function(x){
    if(identical(x, character(0))){
      NA
    }else{
      x
    }
  })
  
  organ <- str_extract_all(pubmedfile, "\r\nAD\\s+- [\\s\\S]*?\r\n\\w")
  organ <- sapply(organ, function(x)str_remove_all(x,"\r\nAD\\s+-\\s|\r\n.*$|\r\n\\s+"))
  organ <- lapply(organ, function(x){
    if(identical(x, character(0))){
      NA
    }else{
      x
    }
  })
  
  yr <- str_extract(str_extract(pubmedfile, "\r\nDP.*"), "\\d{4}")
  yr[yr==""] <- NA
  
  jl <- str_remove(str_extract(pubmedfile, "\r\nTA.*"), "\r\nTA\\s+-")
  jl[jl==""] <- NA
  
  pmid <- str_remove(str_extract(pubmedfile, "\r\nPMID.*"), "\r\nPMID-\\s+")
  pmid[pmid==""] <- NA
  
  mh <- str_extract_all(pubmedfile, "\r\nMH\\s+.*")
  mh <- lapply(mh, function(x){
    if(identical(x, character(0))){
      NA
    }else{
      x
    }
  })
  mh <- sapply(mh, function(x)str_to_lower(str_remove_all(x, "\r\nMH\\s+-\\s+|\\*|/.*$")))
  
  
  majr <- str_extract_all(pubmedfile, "\r\nMH.*\\*.*")
  majr <- sapply(majr, function(x)str_to_lower(str_remove_all(x, "\r\nMH\\s+-\\s+|\\*|/.*$")))
  majr <- lapply(majr, function(x){
    if(identical(x, character(0))){
      NA
    }else{
      x
    }
  })
  
  sh <- str_extract_all(pubmedfile, "\r\nMH\\s+.*/.*")
  sh <- lapply(sh, function(x){
    if(identical(x, character(0))){
      NA
    }else{
      x
    }
  })
  # 为了体现主题词和副主题词间的搭配关系，不仅单独把副主题词抽取出来，而且保留原始的方式
  smh <- lapply(sh, function(x)str_to_lower(str_remove_all(x, "\r\nMH\\s+-\\s")))
  sh <- lapply(sh, function(x)unlist(str_split(str_to_lower(str_remove_all(str_extract(x, "/[^\r]*"),"\\*|^/")), "/")))
  
  ab <- field_extract(pubmedfile, "AB")
  ab <- str_replace_all(ab, "\r\n\\s+", " ")
  ab <- str_remove_all(ab, "^-\\s+")
  ab[ab==""] <- NA
  
  pt <- str_remove(str_extract(pubmedfile, "\r\nPT.*"), "\r\nPT\\s+-\\s+")
  pt[pt==""] <- NA
  
  TEST = sd(c(length(ti), length(au), length(organ), length(yr), length(jl),
              length(pmid), length(mh), length(majr), length(sh), length(smh), length(ab), length(pt)))
  
  if(TEST == 0){
    profile <- new("AB_PUBMED", TI = ti, AU = au,  AD = organ, PDAT = yr, JL = jl,
                   MH = mh, MAJR = majr, AB = ab, PT = pt, SH=sh, SMH=smh, PMID=pmid)
    return(profile)
  }else return("PubMed Document Error")
}
parserAB <- function(docfile, db="PUBMED"){
  db <- match.arg(db, c("WOS", "PUBMED", "CNKI", "CSSCI"))
  res <- switch (db,
                 WOS = wos.parser(docfile),
                 PUBMED = pubmed.parser(docfile),
                 CSSCI = cssci.parser(docfile),
                 CNKI = cnki.parser(docfile)
  )
  return(res)
}

cnkixml.parser <- function(file){
  
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
  
  Ti <- Parser(Bibliography, ".//PrimaryTI//TI[@Lang='zh-CHS']")
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
  
  OrgList <- str_split(str_remove(Parser(Bibliography, ".//Authors//AD"), ";$"), ";")
  
  KwList <- str_split(str_remove(Parser(Bibliography, ".//Keywords//Keyword[@Lang='zh-CHS']"), ";$"), ";;")
  
  KwList <- str_split(KwList, ";")
  
  KwList <- sapply(KwList, function(x)str_remove_all(x, " "))
  
  TEST = sd(c(length(Ti), length(Year), length(AuList), length(OrgList), length(KwList)))
  
  if(TEST == 0){
    profile <- new("cnkiAB", ti = Ti, year = Year, type = Type,
                   au = AuList, AD = OrgList,  kw = KwList, publisher = Publisher)
    return(profile)
  }else return("XML Document Error")
}

wos_profile <- parserAB(wosdoc, db = "WOS")
wos_profile@KW[1:5]
cnki_profile <- parserAB(cnkidoc, db = "CNKI")
cnki_profile@KW[1:5]
cssci_profile <- parserAB(csscidoc, db = "CSSCI")
cssci_profile@KW[1:5]
pubmed_profile <- parserAB(pubmeddoc, db = "PUBMED")
pubmed_profile@MH[1:25]

save(cnki_profile, wos_profile, pubmed_profile, cssci_profile, file = "profile.RData")

## 3.2 data clear
### data filter:将不符合研究条件的数据和标题重复数据剔除
### 将文献类型为Newspaper、会议论文的筛除
### 将无作者无关键词的文献剔除
### 将标题重复文献剔除掉
#### wos的机构字段中含有作者信息，同一单位作者只重复一次地址,
#### 不同单位会标注出来
### 机构和作者对应处理
#### 如果作者和机构不对应的话，将机构list的第一个机构按照作者list长度重复
### 将清洗后的文摘数据保存
setGeneric("ABfilter", function(obj,...) standardGeneric("ABfilter"))
setMethod("ABfilter", signature(obj = "ABprofile"), function(obj,docfile,db){
  if("PT" %in% slotNames(obj)){
    index1 <- which(obj@PT=="Journal Article")
    if(length(index1)==0){
      stop("Miss Journal Article")
    }
    if(length(index1) %in% (1:length(obj@PT))){
      docfile <- docfile[index1]
      obj <- parserAB(docfile, db = db)
    }
  }
  
  index2 <- which(duplicated(obj@TI))
  if(length(index2)!=0){
    docfile <- docfile[-index2]
    obj <- parserAB(docfile, db = db)
  }
  
  if(db=="CNKI"){
    if(!identical(sapply(obj@AU, length), sapply(obj@AD, length))){
      tmp <- which(sapply(obj@AU, length) != sapply(obj@AD, length))
      for(i in 1:length(tmp)){
        obj@AD[[tmp[i]]] <- rep(obj@AD[[tmp[i]]][1], length(obj@AU[[tmp[i]]]))
      }
    }
  }
  ## 将清洗后的文摘数据保存
  readr::write_file(x = str_flatten(docfile, collapse = "\r\nER"),
                    path = paste0("1.query/", db,".txt"))
  return(obj)
  
})

docAB <- ABfilter(obj = cnki_profile, docfile = cnkidoc, db = "CNKI")
docAB2 <- ABfilter(obj = wos_profile, docfile = wosdoc, db = "WOS")
docAB3 <- ABfilter(obj = cssci_profile, docfile = csscidoc, db = "CSSCI")
docAB4 <- ABfilter(obj = pubmed_profile, docfile = pubmeddoc, db = "PUBMED")
# index3 <- which(duplicated(cnki_profile@TI) | is.na(cnki_profile@AU) | is.na(cnki_profile@KW))


# docAB2 <- new("ABprofile", TI = docAB@TI[-index], author = docAB@author[-index],  AD = docAB@AD[-index],
#               year = docAB@year[-index], journal = docAB@journal[-index], keyword = docAB@keyword[-index],
#               reference = docAB@reference[-index], fund = docAB@fund[-index], fund_type = docAB@fund_type[-index],
#               ab = docAB@ab[-index], mh = docAB@mh[-index], ptype = docAB@ptype[-index], pmid = docAB@pmid[-index],
#               sh = docAB@sh[-index], majr = docAB@majr[-index])

### 3.2.3 Synonym merger 同义词合并
ABprofile@KW[1:10]
ABprofile@KW <- sapply(ABprofile@KW, function(x){
  x <- str_replace_all(x, "科研成果转化", "科技成果转化")
  x <- str_replace_all(x, "产学研结合", "产学研合作")
  x <- str_replace_all(x, "科技创新", "技术创新")
  x <- str_replace_all(x, "大学|高等学校|高等院校", "高校")
  x <- str_replace_all(x, "高校技术转移", "大学技术转移")
  
  x <- unique(x)
})

# ## 3.4 将清洗后的文摘数据保存
# readr::write_file(str_flatten(wosdoc[-index2], collapse = "\r\nER"), "1.query/alldata_wos.txt")
# readr::write_file(str_flatten(cnkidoc[-index1], collapse = "\r\nER"), "1.query/alldata_cnki.txt")

### 将清洗后的文摘数据保存方法2
# dupti <- docAB@TI[index]
# dupindex <- sapply(paste0("【来源篇名】", dupti), function(x)which(str_detect(cssci, x)))
# dupindex <- unlist(sapply(dupindex, function(x){
#   ifelse(length(x)==1, x, x[-1])
# }))
#
# tmp <- c()
# tmp2 <- c()
# for(i in 1:length(dupindex)){
#   tmp[i] <- match("-----------------------------------------------------------------------", cssci[dupindex[i]:length(cssci)])
#   # cssci2 <- cssci2[-(dupindex[i]: (dupindex[i]+tmp-1))]
#   tmp2 <- c(tmp2, (dupindex[i]: (dupindex[i]+tmp[i]-1)))
# }
#
# cssci2 <- cssci[-tmp2]
# ifelse(length(cssci2[str_detect(cssci2, "【来源篇名】")]) == length(unique(cssci2[str_detect(cssci2, "【来源篇名】")]))
#        ,writeLines(cssci2, "./1.query/cssci_1804/alldata.txt"), print("still have duplicate TI"))
#

#### 将清洗后的文摘数据保存方法3
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
year_d <- as.data.frame(table(docAB@PDAT))%>%
  arrange(Var1)%>%
  rename(year=Var1, count=Freq)

write.table(year_d,"./6.res/year/year_d.txt", quote = F, col.names = NA, sep = "\t")

ggplot(year_d, aes(x=year, y=count, group=1))+ geom_point(color="#56B4E9",size=5)+
  geom_line()+ xlab("年份/年")+ ylab("发文数量/篇")+
  geom_text(aes(label=count), vjust=0.5, hjust=1)+ ggsave("./6.res/year/year.png")

### 1.4.2 author/AD
author_d <- as.tbl(as.data.frame(table(unlist(docAB2@author)), stringsAsFactors = FALSE))%>%
  arrange(desc(Freq))%>%
  rename(author=Var1)

author1_d <- as.tbl(as.data.frame(table(sapply(docAB2@author, function(x)x[1])), stringsAsFactors = FALSE))%>%
  arrange(desc(Freq))%>%
  rename(author=Var1)


author_d$orgranization <- sapply(author_d$author, function(x){
  tmp1 <- unlist(docAB2@author)
  tmp2 <- unlist(docAB2@AD)
  names(sort(table(tmp2[tmp1 %in% x]), decreasing = T)[1])
})

author1_d$AD <- sapply(author1_d$author, function(x){
  tmp <- sapply(docAB2@author, function(y)y[[1]])
  docAB2@AD[[match(x, tmp)]][1]
})


author_d <- data.frame(author=unlist(docAB2@author), AD=unlist(docAB2@AD))%>%
  dplyr::group_by(author, AD)%>%
  dplyr::mutate(freq = n())%>%
  arrange(desc(freq))%>%
  ungroup()%>%
  unique()

author$author <- as.character(author1$author1)
author$AD <- as.character(author1$AD)

author1 <- data.frame(author1=sapply(docAB2@author, function(y)y[[1]]), AD=sapply(docAB2@AD, function(y)y[[1]]))%>%
  dplyr::group_by(author1, AD)%>%
  dplyr::mutate(freq = n())%>%
  arrange(desc(freq))%>%
  ungroup()%>%
  unique()

author1$author1 <- as.character(author1$author1)
author1$AD <- as.character(author1$AD)


AD_d <- as.tbl(as.data.frame(table(unlist(docAB2@AD)), stringsAsFactors = FALSE))%>%
  arrange(desc(Freq))%>%
  rename(AD=Var1)

write.table(author_d, "./6.res/author/author_d.txt", quote = F, col.names = NA, sep = "\t")
write.table(author1_d, "./6.res/author/author1_d.txt", quote = F, col.names = NA, sep = "\t")
write.table(AD_d, "./6.res/author/AD_d.txt", quote = F, col.names = NA, sep = "\t")

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

reference_d <- tidyr::separate(reference_d, col = 1, sep = "\\.", into = c("author","TI", "journal", "year", "Volume_issue"))
reference_d <- tidyr::separate(reference_d, col = "Volume_issue", sep = ":", into = c("Volume_issue","page"))

reference_d$Volume_issue <- str_remove_all(reference_d$Volume_issue, "\\)$|）$")
reference_d$Volume_issue <- str_replace_all(reference_d$Volume_issue, "（", "(")
reference_d <- tidyr::separate(reference_d, col = "Volume_issue", sep = "\\(", into = c("Volume","issue"))
reference_d$issue <- str_split(reference_d$Volume_issue, "\\(")

write.table(reference_d, "./6.res/journal/reference_d.txt", quote = F, col.names = NA, sep = "\t")

### 1.4.4 keyword



author_d$orgranization <- sapply(author_d$author, function(x){
  # tmp <- match(x,docAB2@author)
  # docAB2@AD[[tmp]][str_detect(docAB2@author[tmp], x)]
  index <- sapply(docAB2@author, function(y)str_detect(y, x))
  tmp <- docAB2@AD[index]
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


keyword_d <- as.tbl(as.data.frame(table(unlist(docAB@KW))))%>%
  arrange(desc(Freq))%>%
  rename(keyword=Var1)

wordcloud

keyword_d$keyword <- as.character(keyword_d$keyword)

arrange(docAB2@keywordsc(Freq))%>%
  rename(keyword=Var1)


write.table(keyword_d, "./6.res/keyword/keyword_d.txt", quote = F, col.names = NA, sep = "\t")

as.tbl(as.data.frame(table(table(unlist(docAB2@keyword))))) %>%
  dplyr::mutate(cumfreq=(cumsum(Freq)/sum(Freq))*100) %>%
  plot_ly(x=~Var1, y=~cumfreq, type = 'scatter', mode = 'lines+markers') %>%
  layout(xaxis = list(TI = "keyword Frequence", tickangle = -45),
         yaxis = list(TI = "Cumulative Frequency(100%)"),
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


cnki <- xmlTreeParse("1.query/cnki_4473/CNKI-636942361833293750.txt", useInternalNodes = T, encoding = "UTF-8")
cnkiAB <- cnki.parser(cnkifilename)



##########
#这个地方如果数据文件的编码不是utf-8会有warnings，且数据量不对，如utf-8-boom就需要转成utf-8
#invalid input found on input connection 'download_3.txt' 这种警告一般是编码问题
#incomplete final line found on 'download_10.txt' 这种警告需要在文件末尾加一行空行
##########
setwd("2.input/")
filename <- dir("./")
cnki <- unlist(lapply(filename, readLines))
file.rename(filename, paste0("download_", 1:length(filename), ".txt"))
setwd("../")

writeLines(cnki, "./1.query/alldata.txt")

ky <- cnki[str_detect(cnki, "K1 ")]
ky <- str_remove_all(ky, "^K1 ")
ky <- tolower(ky)
ky <- str_remove(ky, ";$")
ky <- str_split(ky, ";")

keyword_d <- as.tbl(as.data.frame(table(unlist(ky))))%>%
  arrange(desc(Freq))%>%
  rename(keyword=Var1)
keyword_d$keyword <- as.character(keyword_d$keyword)

write.table(keyword_d, "./6.res/keyword/keyword_d1.txt", quote = F, col.names = NA, sep = "\t")

#### 关键词同义词合并
ky <- sapply(ky, function(x){
  # x <- str_replace_all(x, "科研成果转化", "科技成果转化")
  # x <- str_replace_all(x, "产学研结合", "产学研合作")
  # x <- str_replace_all(x, "科技创新", "技术创新")
  # x <- str_replace_all(x, "大学|高等学校|高等院校", "高校")
  ### 匹配双字节字符[^x00-xff];匹配中文字符[u4e00-u9fa5]
  x <- str_replace_all(x, "中华人民共和国", "中国")
  x <- str_replace_all(x, "农户|农村居民", "农民")
  x <- str_replace_all(x, "贫困农户", "贫困农民")
  x <- str_replace_all(x, "贫困农村", "农村贫困")
  x <- str_replace_all(x, "贫困学生", "贫困生")
  x <- str_replace_all(x, "贫困成因", "贫困原因")
  x <- str_replace_all(x, "贫困户|贫困群体|贫困人口", "贫困人口")
  x <- str_replace_all(x, "贫困大学生", "高校贫困生")
  x <- unique(x)})

keyword_d <- as.tbl(as.data.frame(table(unlist(ky))))%>%
  arrange(desc(Freq))%>%
  rename(keyword=Var1)
keyword_d$keyword <- as.character(keyword_d$keyword)

write.table(keyword_d, "./6.res/keyword/keyword_d2.txt", quote = F, col.names = NA, sep = "\t")

as.tbl(as.data.frame(table(table(unlist(ky))))) %>%
  dplyr::mutate(cumfreq=(cumsum(Freq)/sum(Freq))*100) %>%
  plot_ly(x=~Var1, y=~cumfreq, type = 'scatter', mode = 'lines+markers') %>%
  layout(xaxis = list(TI = "keyword Frequence", tickangle = -45),
         yaxis = list(TI = "Cumulative Frequency(100%)"),
         # margin = list(b = 100),
         showlegend = FALSE)

minfreq <- 10
# 矩阵
res_de <- keyword_d
de.term <- res_de$keyword[res_de$Freq >= minfreq]
# de.term <- intersect(readClipboard(), res_de$Var1)

de <- ky

de_term.list <- sapply(de, function(x)return(intersect(x, de.term)))
de_term.list <- de_term.list[sapply(de_term.list, length)!=0]  

de_term.list[1:5]

tdm <- Matrix(0, nrow = length(de.term), ncol = length(de_term.list), dimnames = list(de.term, names(de_term.list)))
for (i in 1:length(de_term.list)){
  tdm[de_term.list[[i]], i] <- 1
}

write.table(as.matrix(tdm), "./6.res/keyword/tdm.txt", sep = "\t", quote = F, col.names = NA)


com <- Matrix(0, nrow = length(de.term), ncol = length(de.term), dimnames = list(de.term, de.term))
for (i in 1:length(de_term.list)){
  com[de_term.list[[i]], de_term.list[[i]]] <- 1 + com[de_term.list[[i]], de_term.list[[i]]]
}

com[1:5, 1:5]
write.table(as.matrix(com), "./6.res/keyword/com.txt", sep = "\t", quote = F, col.names = NA)

com_citespace <- read.delim("5.project/network201x201.txt", header = FALSE, sep =" ")
com_citespace <- as.matrix(com_citespace)
com_citespace_label <- read.delim("5.project/network201x201Labels.txt", header = FALSE)[,1]
com_citespace_label <- str_remove(com_citespace_label, "^@PHRASE")

dimnames(com_citespace) <- list(com_citespace_label, com_citespace_label)
com_citespace[1:5, 1:5]
write.table(com_citespace, "./6.res/keyword/com_citespace.txt", sep = "\t", quote = F, col.names = NA)

library(igraph)
g1 <- graph.adjacency(com, mode = "undirected", weighted = T, diag = F)
g2 <- graph.adjacency(com_citespace, mode = "undirected", weighted = T, diag = F)
g1
g2

set.seed(1000)
wc1 <- cluster_walktrap(g1, steps = 5)#0.49
wc1
groups(wc1)

wc2 <- cluster_walktrap(g2, steps = 7)#0.49
wc2
groups(wc2)

diag(com)[1]
rowSums(com)

Eindex <- function(com){
  # diag(Em) <- 1
  # for(i in 1:(nrow(Em)-1)){
  #   for(j in (i+1):nrow(Em)){
  #     Em[i,j] <- round(com[i,j]^2/(diag(com)[i]*diag(com)[j]),5)
  #   }
  # }
  Em <- com
  diag(Em) <- 0
  res <- summary(Em)
  res$x <- round(res$x^2/(diag(com)[res$i]*diag(com)[res$j]),5)
  
  c1 <- NULL
  i <- 1
  res2 <- res
  
  ##阈值为0.01，E指数小于0.01认为==0
  while(max(res2$x)>=0.01){
    c2 <- res2[which.max(res2$x),]
    print(c2$x)
    c2_1 <- filter(res2, j==c2$j) %>%
      arrange(desc(x)) %>%
      dplyr::slice(1:9)
    c2_2 <- filter(res2, i==c2$i) %>%
      arrange(desc(x)) %>%
      dplyr::slice(1:9)
    if(nrow(c2_1)>=nrow(c2_2)){
      c2 <- c2_1
    }else{
      c2 <- c2_2
    }
    # c2 <- if_else(nrow(c2_1)>=nrow(c2_2), c2_1, c2_2)
    res2 <- filter(res2, !(i %in%c(c2$i,c2$j)) & !(j %in% c(c2$i,c2$j)))
    c2$cluster <- i
    i <- i+1
    c1 <- rbind(c1,c2)
  }
  
  library(reshape2)
  
  res_cluster <- melt(c1, measure.vars = 1:2, id.vars = 4, value.name = "keyword.index") %>%
    dplyr::select(c(1,3)) %>%
    distinct_all() %>%
    arrange(cluster) %>%
    mutate(keyword = colnames(com)[keyword.index])
  write.table(res_cluster, "./6.res/keyword/res_cluster.txt", sep = "\t", quote = F, col.names = NA)
  
  res_cluster$name <- rowSums(Em)[res_cluster$keyword.index]
  
  group_by(res_cluster, cluster) %>%
    mutate(N <- which.max(name))
  
  
  
  zbplot <- dplyr::group_by(c1, cluster) %>%
    summarise(density=mean(Em[unique(c(i,j)),unique(c(i,j))]),
              centrality=sum(Em[unique(c(i,j)),-unique(c(i,j))]))
  zbplot[8:26,]
  zbplot2 <- zbplot
  write.table(zbplot, "./6.res/keyword/zbplot.txt", sep = "\t", quote = F, col.names = NA)
  zbplot[14,3] <- zbplot[14,3]+100
  zbplot[20,3] <- zbplot[20,3]+30
  zbplot[25,3] <- zbplot[25,3]-20
  zbplot[17,2] <- zbplot[17,2]+0.1
  zbplot[25,3] <- zbplot[25,3]-0.05
  
  zbplot3 <- zbplot
  zbplot$density <- scale(zbplot$density)
  zbplot$centrality <- scale(zbplot$centrality)
  
  ggplot(zbplot,aes(x=centrality, y=density, fill=factor(cluster)))+
    geom_point(size=10, shape=22, alpha = .8)+
    guides(fill=F)+
    geom_hline(yintercept = mean(zbplot$density))+
    geom_vline(xintercept = mean(zbplot$centrality))+
    geom_text(aes(label=cluster))+
    xlab("关注度")+ ylab("新颖度")+
    # annotate("text", 2, -0.1, size = 10, label = "关注度")+
    # annotate("text", -0.1, 2.5, size = 10, label = "新\r\n颖\r\n度")+
    theme(axis.TI = element_text(size = 25),  panel.grid.minor = element_blank(),
          panel.grid.major=element_blank(), panel.background = element_blank())+
    scale_x_continuous(breaks = seq(-1,3.5,0.5))+
    scale_y_continuous(breaks = seq(-1,3.5,0.5))+
    ggsave("./6.res/keyword/战略坐标图.png")
  # geom_point(size=10, shape=22, alpha = .8)+
  # geom_jitter(width=10,height=0.5)
  # geom_label(aes(label=cluster))+
  # geom_jitter(width=10,height=0.5)
  
  Em <- dcast(res, formula = i~j, value.var = "x", fill = 0)
  Em <- Em[,-1]
  rownames(Em) <- rownames(com)
  colnames(Em) <- colnames(com)
  Em <- as.matrix(Em)
  Em[lower.tri(Em)] <- Em[upper.tri(Em)]
  write.table(Em, "./6.res/keyword/emindex.txt", sep = "\t", quote = F, col.names = NA)
  
}


?hclust()
install.packages("factoextra")

save(list=ls(), file = "citespace.Rdata")



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

### 1.4.2 author/AD
author_d <- as.tbl(as.data.frame(table(unlist(docAB2@author)), stringsAsFactors = FALSE))%>%
  arrange(desc(Freq))%>%
  rename(author=Var1)

author1_d <- as.tbl(as.data.frame(table(sapply(docAB2@author, function(x)x[1])), stringsAsFactors = FALSE))%>%
  arrange(desc(Freq))%>%
  rename(author=Var1)


author_d$orgranization <- sapply(author_d$author, function(x){
  tmp1 <- unlist(docAB2@author)
  tmp2 <- unlist(docAB2@AD)
  names(sort(table(tmp2[tmp1 %in% x]), decreasing = T)[1])
})

author1_d$AD <- sapply(author1_d$author, function(x){
  tmp <- sapply(docAB2@author, function(y)y[[1]])
  docAB2@AD[[match(x, tmp)]][1]
})


author_d <- data.frame(author=unlist(docAB2@author), AD=unlist(docAB2@AD))%>%
  dplyr::group_by(author, AD)%>%
  dplyr::mutate(freq = n())%>%
  arrange(desc(freq))%>%
  ungroup()%>%
  unique()

author$author <- as.character(author1$author1)
author$AD <- as.character(author1$AD)

author1 <- data.frame(author1=sapply(docAB2@author, function(y)y[[1]]), AD=sapply(docAB2@AD, function(y)y[[1]]))%>%
  dplyr::group_by(author1, AD)%>%
  dplyr::mutate(freq = n())%>%
  arrange(desc(freq))%>%
  ungroup()%>%
  unique()

author1$author1 <- as.character(author1$author1)
author1$AD <- as.character(author1$AD)


AD_d <- as.tbl(as.data.frame(table(unlist(docAB2@AD)), stringsAsFactors = FALSE))%>%
  arrange(desc(Freq))%>%
  rename(AD=Var1)

write.table(author_d, "./6.res/author/author_d.txt", quote = F, col.names = NA, sep = "\t")
write.table(author1_d, "./6.res/author/author1_d.txt", quote = F, col.names = NA, sep = "\t")
write.table(AD_d, "./6.res/author/AD_d.txt", quote = F, col.names = NA, sep = "\t")

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

reference_d <- tidyr::separate(reference_d, col = 1, sep = "\\.", into = c("author","TI", "journal", "year", "Volume_issue"))
reference_d <- tidyr::separate(reference_d, col = "Volume_issue", sep = ":", into = c("Volume_issue","page"))

reference_d$Volume_issue <- str_remove_all(reference_d$Volume_issue, "\\)$|）$")
reference_d$Volume_issue <- str_replace_all(reference_d$Volume_issue, "（", "(")
reference_d <- tidyr::separate(reference_d, col = "Volume_issue", sep = "\\(", into = c("Volume","issue"))
reference_d$issue <- str_split(reference_d$Volume_issue, "\\(")

write.table(reference_d, "./6.res/journal/reference_d.txt", quote = F, col.names = NA, sep = "\t")

### 1.4.4 keyword



author_d$orgranization <- sapply(author_d$author, function(x){
  # tmp <- match(x,docAB2@author)
  # docAB2@AD[[tmp]][str_detect(docAB2@author[tmp], x)]
  index <- sapply(docAB2@author, function(y)str_detect(y, x))
  tmp <- docAB2@AD[index]
  names(sort(table(unlist(tmp)), decreasing = T)[1])
  # tmp <- as.data.frame(table(unlist(tmp)))%>%
  #   arrange(desc(Freq))
  # as.character(tmp[1,1])
})



write.table(author_d, "./6.res/author/author_d.txt", quote = F, col.names = NA, sep = "\t")
write.table(author1_d, "./6.res/author/author1_d.txt", quote = F, col.names = NA, sep = "\t")



res_au_1$instruc <- sapply(res_au_1$Var1, function(x){
  tmp <- wos_wos[str_detect(wos_wos, paste0("^C1 \\[", x, "\\]"))]
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
  layout(xaxis = list(TI = "keyword Frequence", tickangle = -45),
         yaxis = list(TI = "Cumulative Frequency(100%)"),
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