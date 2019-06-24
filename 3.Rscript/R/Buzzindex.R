Buzzindex <- function(meshterm, timewindow, obj){
  current <- sort(as.integer(names(table(obj@PDAT))), decreasing = TRUE)[1:timewindow]
  term.list.current <- obj@MAJR[!is.na(match(obj@PDAT, current))]
  term.list.current <- term.list.current[term.list.current != "NO Result Found"]
  term.current <- as.data.frame(table(unlist(term.list.current))) %>% 
    arrange(desc(Freq))
  term.current$Var1 <- as.character(term.current$Var1)
  c <- term.current[match(meshterm, term.current[,1]), 2]
  t1 <- which(is.na(c))
  if(length(t1)!=0){c[t1] <- 0}
  n1 <- (c+1)/(term.current[1,2]+1)
  
  term.list.previous <- obj@MAJR[is.na(match(obj@PDAT, current))]
  term.list.previous <- term.list.previous[term.list.previous != "NO Result Found"]
  term.previous <- as.data.frame(table(unlist(term.list.previous))) %>% 
    arrange(desc(Freq))
  
  p <- term.previous[match(meshterm, term.previous[,1]), 2]
  t2 <- which(is.na(p))
  if(length(t2)!=0){p[t2] <- 0}
  n2 <- (p+1)/(term.previous[1,2]+1)
  Buzzindex <- log(c) * (n1/n2)
  if(any(is.infinite(Buzzindex))){
    Buzzindex[is.infinite(Buzzindex)] <- 0
  }
  names(Buzzindex) <- meshterm
  
  return(Buzzindex)
}