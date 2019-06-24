Pubtator_matrix <- function(res.pubtator, type = "com", xentity = NULL, 
                            yentity = NULL, localfreq=1L, globalfreq=1L){
  
  d1 <- as.tbl(res.pubtator[,c(1:3)]) %>% 
    group_by(name) %>% 
    count(pmid,type) %>% 
    mutate(global = n()) %>% 
    ungroup() %>% 
    filter(n>=localfreq & global >= globalfreq) %>% 
    group_by(pmid) %>% 
    arrange(pmid,type,name) 
  
  d1$rank <- group_indices(d1)
    
  term <- unique(d1$name)
  pmidlist <- as.character(unique(d1$pmid))
  list1 <- dlply(res.pubtator, "pmid")
  
  type <- match.arg(type, c("tdm", "com"))
  if(type == "tdm"){
    tdm <- Matrix(0, nrow = length(term), ncol = length(pmidlist), dimnames = list(term, pmidlist))
    for (i in 1:length(pmidlist)){
      tdm[d1$name[d1$rank==i], i] <- d1$n[d1$rank==i]
    }
    if(!is.null(xentity)){
      tdm <- tdm[unique(filter(d1, type%in%xentity)$name), ]
      tdm <- tdm[,colSums(tdm) != 0]
    }
    write.table(as.matrix(tdm), "pub_tdm.txt", sep = "\t", quote = F, col.names = NA)
    return(tdm)
  }else{
    com <- Matrix(0, nrow = length(term), ncol = length(term), dimnames = list(term, term))
    for (i in 1:length(pmidlist)){
      com[d1$name[d1$rank==i], d1$name[d1$rank==i]] <- 1 + com[d1$name[d1$rank==i], d1$name[d1$rank==i]]
    }
    if(!is.null(c(xentity, yentity))){
      if(is.null(xentity)){
        com <- com[, unique(filter(d1, type%in%yentity)$name)] 
      }
      if(is.null(yentity)){
        com <- com[unique(filter(d1, type%in%xentity)$name), ] 
      }
      if(!is.null(xentity) & !is.null(yentity)){
        com <- com[unique(filter(d1, type%in%xentity)$name), unique(filter(d1, type%in%yentity)$name)] 
      }
      com <- com[rowSums(com)!=0, colSums(com)!=0]
    }
    write.table(as.matrix(com), "pub_com.txt", sep = "\t", quote = F, col.names = NA)
    return(com)
  }
}