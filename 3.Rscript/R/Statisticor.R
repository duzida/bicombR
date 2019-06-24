#basic statistics
utils::globalVariables("meshtree")
utils::globalVariables("jourinfo")
setGeneric("Statisticor", function(obj, field, n = 100) standardGeneric("Statisticor"))

setMethod("Statisticor", "ABprofile", function(obj, field, n = 100){
  field <- match.arg(field, slotNames(obj)[-c(1:3,6)])
  
  #publish trends
  if(field == "PDAT"){
    d1 <- as.tbl(as.data.frame(table(obj@PDAT)))
    
    p1 <-  d1 %>% 
      plot_ly(x = ~Var1, y = ~Freq, type = "bar") %>% 
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = "Freq"), 
             showlegend = FALSE) %>%
      add_lines(x = ~Var1, y = lowess(d1)$y)
    
    print(p1)
    write.table(d1, "date_freq.txt", row.names = F, quote = F, sep = "\t")
    return(d1)
  }
  
  #core journal
  if(field == "TA"){
    d2 <- as.tbl(data.frame(journal = obj@TA, ISSN = obj@ISSN, stringsAsFactors = F)) %>% 
      group_by(journal) %>% 
      dplyr::mutate(freq = n()) %>% 
      unique() %>% 
      left_join(jourinfo[,2:3], "ISSN") %>% 
      arrange(desc(freq))
    d2$Impact.Factor[is.na(d2$Impact.Factor)] <- 0
    d2$type <- cut(d2$Impact.Factor, c(-Inf,0,1,10,Inf), 
                   labels = c("unknow","(0,1]","(1,10]","(10,Inf]"))
    d2$Impact.Factor[d2$Impact.Factor == 0] <- NA
    
    p2 <- d2[1:n,] %>% 
      plot_ly(x = ~reorder(journal, freq), y = ~freq, type = "bar",
              color = ~type, hoverinfo = 'text', text = ~paste('TA: ', journal, 
                                                               '</br> IF: ', Impact.Factor,
                                                               '</br> Freq: ', freq)) %>% 
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Freq"))
    
    print(p2)
    write.table(d2[,1:4], "journal_freq.txt", row.names = F, quote = F, sep = "\t")
    return(d2)
  }
  
  #core author
  if(field == "AU"){
    d3 <- as.tbl(as.data.frame(table(sapply(obj@AU, function(x)x[1])))) %>% 
      arrange(desc(Freq))
    
    p3 <- d3[d3$Freq >= 0.749*d3$Freq[1]^0.5,] %>% 
      plot_ly(x = ~reorder(Var1, Freq), y = ~Freq, type = "bar", color = ~factor(Freq)) %>% 
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Freq"))
    
    print(p3)
    write.table(d3, "author1.st_freq.txt", row.names = F, quote = F, sep = "\t")
    return(d3)
  }
  
  #top MeSH term
  if(field == "MH"){
    d4 <- as.tbl(as.data.frame(table(unlist(obj@MH)), stringsAsFactors = F)) %>% 
      left_join(as.data.frame(table(unlist(obj@MAJR)), stringsAsFactors = F), by="Var1") %>% 
      arrange(desc(Freq.x))
    colnames(d4) <- c("MH", "mesh.freq", "majr.freq")
    d4$majr.freq[is.na(d4$majr.freq)] <- 0
    
    p4 <- d4[1:n, ] %>% 
      plot_ly(x = ~reorder(MH,mesh.freq), y = ~mesh.freq, type = "bar", name = "MH") %>% 
      add_trace(y = ~majr.freq, name = "MAJR") %>% 
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Freq"))
    print(p4)
    write.table(d4, "MeSH_freq.txt", row.names = F, quote = F, sep = "\t")
    return(d4)
  }
  
  #top SH term
  if(field == "SH"){
    d5 <- as.tbl(as.data.frame(table(unlist(obj@SH)), stringsAsFactors = F)) %>% 
      arrange(desc(Freq))
    
    p5 <- wordcloud2::wordcloud2(d5)
    
    print(p5)
    write.table(d5, "MAJR_freq.txt", row.names = F, quote = F, sep = "\t")
    return(d5)
  }
  
  #country map
  if(field == "AD"){
    l <- list(color = toRGB("grey"), width = 0.5)
    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator'))
    d6 <- as.tbl(as.data.frame(table(unlist(obj@AD)), stringsAsFactors = F)) %>% 
      arrange(desc(Freq))
    
    p6 <- d6 %>% 
      plot_geo(locationmode = 'country names') %>% 
      add_trace(z = ~Freq, color = ~Freq, colors = RColorBrewer::brewer.pal(3, "Set2"),
                text = ~Var1, locations = ~Var1, marker = list(line = l)) %>%
      colorbar(title = 'Number of articles published') %>%
      layout(title = 'Word map', geo = g)
    
    print(p6)
    write.table(d6, "Country_freq.txt", row.names = F, quote = F, sep = "\t")
    return(d6)
  }
})