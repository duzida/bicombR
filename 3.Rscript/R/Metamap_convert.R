Metamap_convert <- function(metamaptext){
  tctest <- metamaptext[!is.na(str_extract(metamaptext, "^Processing \\d*\\.ti.1|^.*\\d*   C\\d*:"))]
  ti <- which(!is.na(str_extract(tctest, "^Processing \\d*\\.ti.1")))
  ti_pmid <- str_replace(tctest[ti], "^Processing (\\d*)\\.ti.1.*", "\\1")
  pmid <- c(rep(ti_pmid, diff(c(ti, length(tctest)))-1), ti_pmid[length(ti_pmid)])
  tt <- str_split(tctest[-ti], ":")
  cui <- sapply(tt, function(x){unlist(str_split(x[1], "[^^]   "))[2]})
  metamap.d <- data.frame(pmid, cui, stringsAsFactors = F)
  metamap.d2 <-  plyr::count(metamap.d, colnames(metamap.d))
  metamap.d2$idf <- log2(length(ti)/(table(metamap.d2$cui)[match(metamap.d2$cui, names(table(metamap.d2$cui)))]))
  metamap.d2$tf_idf <- metamap.d2$freq * metamap.d2$idf
  metamap.d2$tf_idf <- (metamap.d2$tf_idf-min(metamap.d2$tf_idf))/(max(metamap.d2$tf_idf)-min(metamap.d2$tf_idf))
  colnames(metamap.d2)[3] <- "tf"
  return(metamap.d2)
}