# calculate similarity between node, mesh term and articles based information content method
utils::globalVariables("meshtree")
setGeneric("Simmatrix", function(obj, ...) standardGeneric("Simmatrix"))
setMethod("Simmatrix", "ABprofile", function(obj, type="mesh"){
  type <- match.arg(type, c("node", "mesh", "doc"))
  
  majrlist <- obj@MAJR
  names(majrlist) <- obj@PMID
  d1 <- plyr::count(unlist(majrlist)) %>% 
    mutate(x=as.character(x)) %>% 
    rename(mesh=x) %>% 
    inner_join(meshtree[,c(1,3:4)], "mesh")  %>% 
    unique()
  
  N <- sum(unique(d1[,1:2])$freq)
  t <- unique(d1$mesh)
  majrlist <- sapply(majrlist, function(x)return(intersect(x, t)))
  
  CountIv <- function(v){
    iv <- sapply(v, function(x){
      des <- grep(x, d1$vname, fixed = T)
      return(-log2(sum(d1$freq[des])/N))
    })
    return(iv)
  }
  d1$iv <- CountIv(d1$vname)
  
  SIMN <- Matrix(0, nrow = nrow(d1), ncol = nrow(d1), dimnames = list(d1$vname, d1$vname))
  
  Ancestor <- function(v1, v2){
    if(v1 == v2){return(v1)}
    t <- strsplit(c(v1, v2), "\\.")
    i <- 1
    while((i <= min(length(t[[1]]), length(t[[2]]))) && (t[[1]][i] == t[[2]][i])){
      i <- i + 1
    }
    ancestor <- ifelse((i-1) == 0, substr(v1, 1, 1), substr(v1, 1, 4*(i-1)-1))
    return(ancestor)
  }
  
  VSim <- function(x){
    v1 <- d1$vname[x]
    v2 <- d1$vname[-(1:x)]
    res <- rep(0, length(v2))
    tmp1 <- which(d1$meshclass[x] == d1$meshclass[-(1:x)])
    if(length(tmp1) == 0){return(res)}
    ancestor <- sapply(v2[tmp1], function(p)Ancestor(v1, p))
    ancestoriv <- CountIv(unique(ancestor))
    dist <- d1$iv[x] + d1$iv[-(1:x)][tmp1] - 2* ancestoriv[ancestor]
    res[tmp1] <- round(exp(-dist/3), 3)
    return(res)
  }
  
  SIMN[lower.tri(SIMN)] <- unlist(lapply(1:(nrow(SIMN)-1), VSim))
  SIMN <- t(SIMN) + SIMN
  diag(SIMN) <- 1
  
  if(type == "node"){
    return(SIMN)
  }
  
  SIMM <- Matrix(0, nrow = length(t), ncol = length(t), dimnames = list(t, t))

  t1 <- sapply(t, function(x)which(d1$mesh %in% x))
  t2 <- sapply(t1, function(x)unique(d1$meshclass[x]))
  
  
  AMM <- function(m){
    tmp <- sum(dim(m))
    res <- sum(unlist(sapply(1:2, function(p)apply(m, p, max))))
    return(round(res/tmp, 3))
  }
  
  MSim <- function(x){
    res <- rep(0, length(t)-x)
    tmp1 <- t2[-(1:x)]
    tmp2 <- which(sapply(tmp1, function(p){length(intersect(p, t2[[x]]))}) != 0) 
    res[tmp2] <- sapply(tmp2, function(i){
      AMM(SIMN[t1[[x]], t1[[x+i]], drop = F])
    })
    return(res)
  }
  
  SIMM[lower.tri(SIMM)] <- unlist(lapply(1:(nrow(SIMM)-1), MSim))
  SIMM <- t(SIMM) + SIMM
  diag(SIMM) <- 1
  
  if(type == "mesh"){
    return(SIMM)
  }
  
  SIMP <- Matrix(0, nrow = length(majrlist), ncol = length(majrlist), 
                 dimnames = list(obj@PMID, obj@PMID))
  
  PSim <- function(x){
    res <- rep(0, nrow(SIMP)-x)
    res <- sapply((x+1):nrow(SIMP), function(i){
      AMM(SIMM[majrlist[[x]], majrlist[[i]], drop = F])
    })
    return(res)
  }
  
  SIMP[lower.tri(SIMP)] <- unlist(lapply(1:(nrow(SIMP)-1), PSim))
  SIMP <- t(SIMP) + SIMP
  diag(SIMP) <- 1
  
  if(type == "doc"){
    return(SIMP)
  }
  
})