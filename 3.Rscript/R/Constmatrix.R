#Construct matrix
utils::globalVariables("meshtree")
setGeneric("Constmatrix", function(obj, ...) standardGeneric("Constmatrix"))
setMethod("Constmatrix", "ABprofile", function(obj, field="MAJR", type="com", minfreq=1L, plot=F){
  field <- match.arg(field, slotNames(obj)[7:11])
  term.list <- slot(obj, field)
  names(term.list) <- slot(obj, "PMID")
  term.list <- term.list[term.list != "NO Result Found"]
  term <- unique(unlist(term.list))
  
  if(field %in% c("MAJR", "MH")){
    invalid_term <- term[is.na(match(term, meshtree$mesh))]
    term <- setdiff(term, invalid_term)
    term.list <- sapply(term.list, function(x)return(setdiff(x, invalid_term)))
    if(length(invalid_term) != 0){
      warning(paste("terms:'", paste0(invalid_term, collapse = ";"), 
                    "'can't found in meshtree2017 would been removed"))
    }
  }
  
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