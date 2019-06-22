URLcontent <- function(pmidlist){
  opts <- list(connecttimeout = 10)
  hg <- RCurl::basicHeaderGatherer()#头部信息
  opts$headerfunction <- hg$update
  tg <- RCurl::basicTextGatherer()##字符串形式
  opts$writefunction <- tg$update
  
  e <- getURLContent(url = paste("https://www.ncbi.nlm.nih.gov/CBBresearch/Lu/Demo/RESTful/tmTool.cgi/Bioconcept/", 
                                 paste(pmidlist, collapse = ","), "/BioC/", sep = ""), .opts = opts)
  res.txt <- as.character(tg$value())
  rm(opts,hg,tg,e)
  
  return(res.txt)
}

PubtatorAPI <- function(pmidlist){
  res.txt <- URLcontent(pmidlist)
  
  status <- str_extract(res.txt, "\\[Error\\] : PMID:\\d+")
  errinfo <- list()
  i <- 1
  pureid <- pmidlist
  
  while(!is.na(status)){
    warning(status)
    wrongid <- str_extract(status, "\\d+")
    pureid <- pureid[pureid!=wrongid]
    errinfo[[i]] <- "NO data"
    i <- i+1
    Sys.sleep(0.01)
    res.txt <- URLcontent(pureid)
    status <- str_extract(res.txt, "\\[Error\\] : PMID:\\d+")
  }
  
  res.txt <- unlist(str_split(res.txt, "\n<document>\n"))
  res.txt <- paste0("\n<document>\n", res.txt[-1])
  res.txt[length(res.txt)] <- str_replace(res.txt[length(res.txt)],"\n</collection>\n", "")
  
  annota.list <- lapply(res.txt, function(q){
    x <- tryCatch(xmlTreeParse(q, asText = TRUE, useInternalNodes = T, error = NULL),
                  XMLError = function(e) {})
    if(is.null(x)){return("NO data")}
    if(length(getNodeSet(x, "//annotation")) == 0){return("NO data")}
    
    pmid <- xmlValue(getNodeSet(x, "/document/id")[[1]])  
    location <- xmlSApply(getNodeSet(x, "//annotation/location"), function(q)xmlGetAttr(q,"offset"))
    name <- xmlSApply(getNodeSet(x, "//annotation/text"), xmlValue)
    type <- xmlSApply(getNodeSet(x, "//annotation/infon[1]"), xmlValue)
    type <- str_replace(type, "\\n", "")
    tmp <- getNodeSet(x, "//annotation")
    id <- sapply(tmp, function(p){
      t1 <- getNodeSet(p, ".//infon[2]")
      if(length(t1) == 0){
        return(NA)
      }else{
        return(xmlValue(t1[[1]]))
      }
    })
    
    taxonomy <-  sapply(tmp, function(p){
      t1 <- getNodeSet(p, ".//infon[2]")
      if(length(t1) == 0){
        return(NA)
      }else{
        return(xmlGetAttr(t1[[1]], "key"))
      }
    })
    
    return(data.frame(pmid=rep(pmid, length(id)), type,name,id,taxonomy,location, stringsAsFactors = F) %>%
             arrange(type))
  })
  
  return(c(annota.list, errinfo))
}


Pubtator <- function(pmidlist){
  if(length(pmidlist) <= 100){
    res <- PubtatorAPI(pmidlist)
  }else{
    res <- list()
    for(i in 1:floor(length(pmidlist)/100)){
      res[(1+100*(i-1)):(100*i)] <- PubtatorAPI(pmidlist[(1+100*(i-1)):(100*i)])
    }
    if(length(pmidlist)%%100 != 0){
      res[(1+100*i):length(pmidlist)] <- PubtatorAPI(pmidlist[(1+100*i):length(pmidlist)])
    }
  }
  res <- res[res!="NO data"]
  if(length(res)==0){
    return("NO data")
  }else{
    return(ldply(res, .id="pmid"))
  }
}