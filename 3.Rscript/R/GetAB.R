#Get PubMed Abstracts
setClass("eutils", 
         slots = list(host = "character", db = "character", retmax = "character", 
                               rettype = "character", retmode = "character", email = "character", tool = "character"), 
         prototype = list(db = "pubmed", retmode = "xml", email = "gerhard.schofl%40gmail.com", tool = "reutils"))
setClass("search", contains="eutils", 
         representation = list(term = "character"), 
         prototype = list(host = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi", 
                          name = "esearch", rettype = "count", retmax = "100"))
setClass("fetch", contains="eutils", 
         representation = list(id = "character"), 
         prototype = list(host = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi"))

#esearch Get(nchar(term)<100) OR POST(>100)
#esearch retmax(<100,000)
#efetch Get(length(uid)<200) OR Post(>200)
setGeneric("Extractor", function(obj,...) standardGeneric("Extractor"))

setMethod("Extractor", "search", function(obj,...){
  if (is.null(obj@term)) {
    stop("No query term provided")
  }
  if (length(obj@term) > 1L) {
    obj@term <- paste(obj@term, collapse = " OR ")
  }
  obj@term <- paste0(obj@term, " AND English[Filter]")#only extract english articles
  
  field <- list(term = obj@term, db = obj@db, retmax = obj@retmax, rettype = obj@rettype, 
                retmode = obj@retmode, email = obj@email, tool = obj@tool)
  opts <- list(connecttimeout = 10)
  hg <- RCurl::basicHeaderGatherer()
  opts$headerfunction <- hg$update
  tg <- RCurl::basicTextGatherer()
  opts$writefunction <- tg$update
  
  if(nchar(obj@term) < 100){
    url.field <- paste(RCurl::curlEscape(names(field)), RCurl::curlEscape(field), sep = "=", 
                       collapse = "&")
    url <- paste(obj@host, url.field, sep = "?")
    e <- RCurl::getURLContent(url = url, .opts = opts)
  }else{
    e <- RCurl::postForm(obj@host, .params = field,
                         .opts = opts, style = "POST")
  }
  res.txt <- as.character(tg$value())
  res.xml <- xmlTreeParse(res.txt, asText = TRUE, useInternalNodes = T) 
  if(obj@rettype == "count"){
    return(xpathSApply(res.xml, "/eSearchResult/Count", xmlValue))
  }else{
    return(xpathSApply(res.xml, "//Id", xmlValue))
  }
})

setMethod("Extractor", "fetch", function(obj,...){
  field <- list(id = paste(obj@id, collapse = ","), db = obj@db, retmax = obj@retmax, 
                rettype = obj@rettype, retmode = obj@retmode, email = obj@email, tool = obj@tool)
  opts <- list(connecttimeout = 10)
  hg <- RCurl::basicHeaderGatherer()
  opts$headerfunction <- hg$update
  tg <- RCurl::basicTextGatherer()
  opts$writefunction <- tg$update
  
  if(length(obj@id) <= 200){
    url.field <- paste(RCurl::curlEscape(names(field)), RCurl::curlEscape(field), sep = "=", 
                       collapse = "&")
    url <- paste(obj@host, url.field, sep = "?")
    e <- RCurl::getURLContent(url = url, .opts = opts)
  }else{
    e <- RCurl::postForm(obj@host, .params = field,
                         .opts = opts, style = "POST")
  }
  res.txt <- as.character(tg$value())
  res.xml <- xmlTreeParse(res.txt, asText = TRUE, useInternalNodes = T, encoding = "UTF-8") 
  return(res.xml)
})

GetAB <- function(query, format = "pmidlist"){
  format <- match.arg(format, c("pmidlist", "XML"))
  s <- new("search", term = query)
  cat("we only extract English articles\n")
  s@retmax <- Extractor(s)
  s@rettype<- "uilist"
  pmidlist <- Extractor(s)
  if(format == "pmidlist"){
    return(pmidlist)
  }else{
    if(length(pmidlist) > 10000){
      stop("Extraction limit: 10000 records")
    }
    f <- new("fetch", id = pmidlist, retmax = as.character(length(pmidlist)))
    return(Extractor(f))
  }
}
