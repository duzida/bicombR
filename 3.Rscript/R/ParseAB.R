# parse the XML abstract
utils::globalVariables("AD.pattern")
ParseAB <- function(doc){
  
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
