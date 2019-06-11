# Load libraries
library(igraph)
library(dplyr)
library(stringr)
library(XML)
library(Matrix)

#Create Results directory
dir.create("./res", showWarnings = FALSE, recursive = T)

# set global options
options(stringsAsFactors = FALSE)

## 1.parse AB
setClass("ABprofile", representation(PMID = "character", TI = "character", AB = "character", 
                                     TA = "character", PDAT = "character", ISSN = "character", 
                                     MH = "list", SH = "list", MAJR = "list", AD = "list", AU = "list"))

ParseAB <- function(file){
  
  doc <- xmlTreeParse(file, useInternalNodes = T)
  
  parse <- function(xmlnodeset, path){
    sapply(xmlnodeset, function(x){
      temp = xpathSApply(x, path, xmlValue)
      if(length(temp)==0){
        return("NO Result Found")
      }else{
        return(temp)
      }
    })
  }
  
  ArticleList <- getNodeSet(doc, "//PubmedArticle")
  
  PmidList <- parse(ArticleList, ".//MedlineCitation/PMID[1]")
  
  TitleList <- parse(ArticleList, ".//ArticleTitle")
  
  AbstractList <- sapply(parse(ArticleList, ".//AbstractText"), function(x)paste0(x, collapse = " "))
  
  JournalList <- parse(ArticleList, ".//ISOAbbreviation")
  ISSN <- parse(ArticleList, ".//MedlineJournalInfo/ISSNLinking")
  
  YearList <- parse(ArticleList, ".//PubDate")
  YearList <- sapply(YearList, function(x){
    if(x != "NO Result Found"){
      str_sub(x, 1, 4)
    }
  })
  names(YearList) <- NULL
  
  MeshList <- parse(ArticleList, ".//MeshHeading//DescriptorName")
  
  SubmeshList <- sapply(parse(ArticleList, ".//QualifierName"), unique)
  
  MajrList <- sapply(ArticleList, function(x){
    tmp <- getNodeSet(x, ".//DescriptorName[@MajorTopicYN='Y'] | .//QualifierName[@MajorTopicYN='Y']")
    if(length(tmp) == 0){
      return("NO Result Found")
    }else{
      sapply(unique(sapply(tmp, xmlParent)), function(x)xpathSApply(x, ".//DescriptorName", xmlValue))
    }
  })
  
  AuthorList <- parse(ArticleList, ".//AuthorList/Author/LastName | .//AuthorList/Author/ForeName")
  AuthorList <- sapply(AuthorList, function(x){
    if(x[1] != "NO Result Found"){
      return(paste(x[(1:length(x)) %% 2 == 1], x[(1:length(x)) %% 2 == 0], sep = " "))
    }else{
      return(x)
    }
  })
  
  CountryList <- str_extract_all(parse(ArticleList, ".//AuthorList//Author/AffiliationInfo"), AD.pattern)
  CountryList <- sapply(CountryList, function(x){
    if(length(x) == 0){return("NO Result Found")}
    tmp <- str_replace(x, "United States of America", "USA")
    tmp <- str_replace(tmp, "United Kingdom|England", "UK")
    tmp <- str_replace(tmp, "Hong Kong", "China")
    return(unique(tmp))
  })
  
  TEST = sd(c(length(PmidList), length(AbstractList), length(MeshList), length(MajrList), length(CountryList), length(ISSN), 
              length(JournalList), length(AuthorList), length(TitleList), length(SubmeshList), length(YearList)))
  
  if(TEST == 0){
    profile <- new("ABprofile", PMID = PmidList, TI = TitleList, AB = AbstractList, PDAT = YearList,
                   TA = JournalList, ISSN = ISSN, MH = MeshList, MAJR = MajrList, SH = SubmeshList, 
                   AU = AuthorList, AD = CountryList)
    return(profile)
  }else return("XML Document Error")
}

AD.pattern <- as.vector(read.delim("../../科研/BS统计/热点分析/file/AD2.txt", as.is = T, header = F)[1,])
jourinfo <- read.delim("../../科研/BS统计/热点分析/file/2015JCR.txt", as.is = T)
meshtree <- read.delim("../../科研/BS统计/热点分析/file/MESHTREE20162.txt", as.is=T, header = T, row.names = 1)

obj <- ParseAB("pubmed_result.xml")

## 3.construct matrix
setGeneric("Constmatrix", function(obj, ...) standardGeneric("Constmatrix"))
setMethod("Constmatrix", "ABprofile", function(obj, field="MAJR", type="tdm", minfreq=1L, plot=F, path=NULL){
  field <- match.arg(field, slotNames(obj)[7:11])
  term.list <- slot(obj, field)
  names(term.list) <- slot(obj, "PMID")
  term.list <- term.list[term.list != "NO Result Found"]
  term <- unique(unlist(term.list))
  
  # if(field %in% c("MAJR", "MH")){
  #   invalid_term <- term[is.na(match(term, meshtree$mesh))]
  #   term <- setdiff(term, invalid_term)
  #   term.list <- sapply(term.list, function(x)return(setdiff(x, invalid_term)))
  #   if(length(invalid_term) != 0){
  #     warning(paste("terms:'", paste0(invalid_term, collapse = ";"), 
  #                   "'can't found in meshtree2017 would been removed"))
  #   }
  # }
  
  if(minfreq != 1){
    df <- as.tbl(as.data.frame(table(unlist(term.list)), stringsAsFactors = F))
    term <- df$Var1[df$Freq >= minfreq]
    term.list <- sapply(term.list, function(x)return(intersect(x, term)))
    term.list <- term.list[sapply(term.list, length)!=0]
  }
  
  type <- match.arg(type, c("tdm", "com"))
  if(type == "tdm"){
    res <- Matrix(0, nrow = length(term), ncol = length(term.list), dimnames = list(term, names(term.list)))
    for (i in 1:length(term.list)){
      res[term.list[[i]], i] <- 1
    }
    write.table(as.matrix(res), "tdm.txt", sep = "\t", quote = F, col.names = NA)
  }else{
    res <- Matrix(0, nrow = length(term), ncol = length(term), dimnames = list(term, term))
    for (i in 1:length(term.list)){
      res[term.list[[i]], term.list[[i]]] <- 1 + res[term.list[[i]], term.list[[i]]]
    }
    write.table(as.matrix(res), "com.txt", sep = "\t", quote = F, col.names = NA)
  }
  
  #heatmap OR 3D plot 
  if(plot == T){
    if(type == "com"){
      if(isSymmetric(res)){
        res[lower.tri(res)] <- 0
      }
      res.df <- summary(res)
      res.df$i <- factor(res.df$i, labels = rownames(res)[sort(unique(res.df$i))])
      res.df$j <- factor(res.df$j, labels = colnames(res)[sort(unique(res.df$j))])
      res.df$x <- factor(res.df$x)
      
      p2 <- res.df %>% 
        plot_ly(x = ~i, y = ~j, z = ~x, colors = RColorBrewer::brewer.pal(3, "Set2"), 
                color = ~x, mode = "markers", type = "scatter3d") %>%
        add_markers()
      print(p2)
    }else{
      p1 <- plot_ly(x=colnames(res), y=rownames(res), z = as.matrix(res), 
                    type = "heatmap", colors = colorRamp(c("white", "red")))
      print(p1)
    }
  }
  
  return(res)
  
})


names(obj@MAJR) <- obj@PMID
term.list <- obj@MAJR[obj@MAJR != "NO Result Found"]

minfreq.df <- as.tbl(as.data.frame(table(table(unlist(term.list))), stringsAsFactors = F)) %>% 
  arrange(desc(as.integer(Var1))) %>% 
  dplyr::mutate(g1=cumsum(as.integer(Freq)*as.integer(Var1))) %>% 
  dplyr::mutate(g2=cumsum(as.integer(Freq))^2)

minfreq <- as.integer(with(minfreq.df, Var1[which(g1 < g2)[1]-1]))

tdm <- Constmatrix(obj, field="MAJR", type="tdm", minfreq=minfreq, plot=F)
com <- Constmatrix(obj, field="MAJR", type="com", minfreq=minfreq, plot=F)

mhfreq <- as.tbl(as.data.frame(table(unlist(term.list)))) %>% 
  arrange(desc(Freq)) %>% 
  dplyr::mutate(g1=round((cumsum(Freq)/sum(Freq))*100,4)) %>% 
  filter(Freq >= 15)
write.table(mhfreq, "高频词.txt", sep = "\t", quote = F, col.names = NA)

rownames(com)[c(10,35,11,34,31,53,41)]
type <- data.frame(term=rownames(com), cluster=1)
type$cluster[c(10,35,11,34,31,53,41)] <- 1
type$cluster[c(3,33,45)] <- 2
type$cluster[c(12,42,39,25,44)] <- 3
type$cluster[c(30,56,50,9,17,54,23,55)] <- 4
type$cluster[c(7,8,15,26)] <- 5
type$cluster[c(1,4,37,47,20)] <- 6
type$cluster[c(38,52,16,22,14,21,46,43,36,28,13,24,6,48,32)] <- 7
type$cluster[c(2,58,18,27,57,51,29,40,5,19,49)] <- 8

type <- arrange(type, cluster)
# rowMeans(com[type$term[type$cluster==1], type$term[type$cluster==1]])
# type$"粘合力" <- unlist(sapply(1:8, function(x){
#   rowMeans(com[type$term[type$cluster==x], type$term[type$cluster==x]])
# }))

type$"粘合力" <- rowMeans(com[type$term, type$term])
write.table(type, "type.txt", sep = "\t", quote = F, col.names = NA)


