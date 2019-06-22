# construct the multi-dimension sub-matrix
utils::globalVariables("meshtree")
Submatrix <- function(m, type, select="st", xclass, yclass=NULL){
  if(is.null(xclass)){stop("invalid xclass")}
  type <- match.arg(type, c("com", "tdm"))
  select <- match.arg(select, c("meshclass", "st", "sg"))
  
  tmp <- switch(select, 
                meshclass=4,
                st=5,
                sg=6)
  
  d <- as.tbl(data.frame(mesh = rownames(m), stringsAsFactors = F)) %>% 
    left_join(meshtree[,c(3,tmp)], "mesh") %>% 
    unique()
  
  if(type=="tdm"){
    if(!is.null(yclass)){
      warning("invalid yclass")
    }
    m <- m[unique(filter(d, t(d[,2])%in%xclass)$mesh), ]
    m <- m[,colSums(m) != 0]
    write.table(as.matrix(m), "m.txt", sep = "\t", quote = F, col.names = NA)
    return(m)
  }else{
    if(is.null(yclass)){
      m <- m[unique(filter(d, t(d[,2])%in%xclass)$mesh), ]
    }else{
      m <- m[unique(filter(d, t(d[,2])%in%xclass)$mesh), unique(filter(d, t(d[,2])%in%yclass)$mesh)]
    }
    m <- m[rowSums(m)!=0, colSums(m)!=0]
    write.table(as.matrix(m), "m.txt", sep = "\t", quote = F, col.names = NA)
    return(m)
  }
}