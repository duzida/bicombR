# Load libraries
library(dplyr)
library(stringr)
library(Matrix)
library(plotly)

# Create 01_Results directory
dir.create("./res2", showWarnings = FALSE)

# set global options
options(stringsAsFactors = FALSE)

# read data
wosfilename <- dir("文献集/")

# d1 <- readLines("文献集/1.txt")
# d2 <- readLines("文献集/2.txt")
# d3 <- readLines("文献集/3.txt")
# d4 <- readLines("文献集/4.txt")
# d5 <- readLines("文献集/5.txt")

wos <- function(filename){
  file <- readLines(filename)
  r <- file[str_detect(file, "^DE")]
  r <- str_remove(r, "\t")
  r <- str_remove(r, "^DE ")
  r <- str_to_lower(r)
  r <- str_split(r, ";")
  
  r <- lapply(r, function(x){
    tmp <- str_replace(x, "(\\w+)(\\(.*\\))", "\\1")
    tmp <- str_replace_all(tmp, "(\\w+)(<.*>)", "\\1")
    tmp <- str_remove(tmp, "[<>\\(\\)]")
    tmp <- str_remove(tmp, "^ ")
    tmp <- str_remove(tmp, " $")
    tmp <- str_replace(tmp, "cultural", "culture")
    tmp <- str_replace(tmp, "industries cultural industries", "industry")
    tmp <- str_replace(tmp, "contents", "content")
    tmp <- str_replace(tmp, "creativity", "creative")
    tmp <- str_replace(tmp, "organizational", "organization")
  })

  return(r)
}

setwd("文献集/")
term.list <- lapply(wosfilename, wos)
setwd("../")
term.list <- c(term.list[[1]], term.list[[2]], term.list[[3]], term.list[[4]], term.list[[5]])
length(term.list)

freaq <- as.tbl(as.data.frame(table(unlist(term.list)))) %>% 
  arrange(desc(Freq))

term <- as.vector(freaq$Var1)

res <- Matrix(0, nrow = length(term), ncol = length(term), dimnames = list(term, term))
for (i in 1:length(term.list)){
  res[term.list[[i]], term.list[[i]]] <- 1 + res[term.list[[i]], term.list[[i]]]
}

write.table(freaq, "res2/词频.txt", sep = "\t", quote = F, col.names = NA)

data.table::fwrite(as.data.frame(as.matrix(res)),
                   file="res2/共现矩阵.txt", 
                   quote = F, row.names = T, col.names = T, sep = "\t")


# method 1
minfreq.df <- as.tbl(as.data.frame(table(table(unlist(term.list))), stringsAsFactors = F)) %>% 
  arrange(desc(as.integer(Var1))) %>% 
  dplyr::mutate(g1=cumsum(as.integer(Freq)*as.integer(Var1))) %>% 
  dplyr::mutate(g2=cumsum(as.integer(Freq))^2)

minfreq <- as.integer(with(minfreq.df, Var1[which(g1 < g2)[1]-1]))
as.data.frame(filter(freaq, Freq>=minfreq))


# method 2
p1 <- as.tbl(as.data.frame(table(table(unlist(term.list))))) %>% 
  dplyr::rename(occurence_frequence=Var1) %>% 
  dplyr::mutate(cumfreq=cumsum(Freq)/sum(Freq)) %>% 
  plot_ly(x=~occurence_frequence, y=~cumfreq, type = 'scatter', mode = 'lines+markers')
print(p1)

term2 <- as.vector(freaq$Var1[freaq$Freq >= 6])
term.list2 <- sapply(term.list, function(x)return(intersect(x, term2)))
term.list2 <- term.list2[sapply(term.list2, length)!=0]
term.ST <- term.list2
com <- Matrix(0, nrow = length(term2), ncol = length(term2), dimnames = list(term2, term2))
for (i in 1:length(term.ST)){
  com[term.ST[[i]], term.ST[[i]]] <- 1 + com[term.ST[[i]], term.ST[[i]]]
}
data.table::fwrite(as.data.frame(as.matrix(com)),
                   file="res2/高频词共现矩阵.txt", 
                   quote = F, row.names = T, col.names = T, sep = "\t")

write.table(as.data.frame(filter(freaq, Freq>=6)), "res2/高频词词频.txt", sep = "\t", quote = F, col.names = NA)

library(tm)
tm::stemCompletion(term, c("cultural industry", "creative industry"))
stemDocument(term[1:96])

tmpcom <- res[match(as.character(freaq$Var1[1:96]), rownames(res)), as.character(freaq$Var1[1:96])]

write.table(ff, row.names=F, col.names=F, sep = "\t", "res2/top86.txt", quote=F)

#词义合并
# hallyu korean wave;content content industry; culture content culture content industry;culture industry;globalization,glocalization
tmpcom["hallyu",] <- tmpcom["hallyu",]+ tmpcom["korean wave",]
match("korean wave", rownames(tmpcom))#5
tmpcom <- Matrix::triu(tmpcom[-5, -5])+t(Matrix::triu(tmpcom[-5, -5]))
match("culture industry", rownames(tmpcom))
tmpcom <- tmpcom[-1, -1]

tmpcom["globalization",] <- tmpcom["globalization",] + tmpcom["glocalization",]
match("glocalization", rownames(tmpcom))#49
tmpcom <- Matrix::triu(tmpcom[-49, -49])+t(Matrix::triu(tmpcom[-49, -49]))

# match("innovation", rownames(tmpcom))#87
# tmpcom <- tmpcom[-87, -87]

data.table::fwrite(as.data.frame(as.matrix(tmpcom)),
                   file="res2/86com2.txt",
                   quote = F, row.names = T, col.names = T, sep = "\t")

###
match(c("culture content industry","industry", "image", "hybridity"), rownames(tmpcom))
tmpcom <- tmpcom[-c(44,64:66),-c(44,64:66)]
tmpcom["game industry", ] <- tmpcom["game industry",]+ tmpcom["game",]
match("game", rownames(tmpcom))#82
tmpcom <- Matrix::triu(tmpcom[-82, -82])+t(Matrix::triu(tmpcom[-82, -82]))


tmpcom["culture content", ] <- tmpcom["culture content",]+ tmpcom["content",]
match("content", rownames(tmpcom))#19
tmpcom <- Matrix::triu(tmpcom[-19, -19])+t(Matrix::triu(tmpcom[-19, -19]))
# tmpcom <- tmpcom[-87, -87]

tmpcom["creative industry", ] <- tmpcom["creative industry",]+ tmpcom["creative",]
match("creative", rownames(tmpcom))#9
tmpcom <- Matrix::triu(tmpcom[-9, -9])+t(Matrix::triu(tmpcom[-9, -9]))
# tmpcom <- tmpcom[-87, -87]
# diag(tmpcom) <- diag(tmpcom)/32


ff <- freaq[1:96,]
ff<- ff[-match(c("culture industry","culture content industry","industry", "image", "hybridity"), ff$Var1),]
ff <- as.data.frame(ff)
rownames(ff) <- ff$Var1
ff["hallyu",2] <- ff["hallyu",2]+ff["korean wave",2]
ff["globalization",2] <- ff["globalization",2]+ff["glocalization",2]
ff["creative industry",2] <- ff["creative industry",2]+ff["creative",2]
ff["culture content",2] <- ff["culture content",2]+ff["content",2]
ff["game industry",2] <- ff["game industry",2]+ff["game",2]
ff <- ff[-c(match(c("korean wave", "glocalization", "creative", "content", "game"), rownames(ff))),]
diag(tmpcom) <- 0
as.numeric(rowSums(tmpcom))
identical(sort(rownames(tmpcom)), sort(as.character(ff$Var1)))
