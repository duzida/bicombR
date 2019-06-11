# Load libraries
library(Matrix)
library(dplyr)
library(stringr)
# library(igraph)

#Create Results directory
# dir.create("./res", showWarnings = FALSE, recursive = T)

# set global options
options(stringsAsFactors = FALSE)
# options(encoding="utf-8")

dat <- readLines("savedrecs.txt")
ky <- which(str_detect(dat, "^ID "))
res <- list()
for(i in ky){
  tmp <- which(str_detect(dat[i:length(dat)], "^AB "))[1]
  t <- str_remove(dat[i:(i+tmp-2)], "^   ")
  t <- paste0(t, collapse = " ")
  t <- str_remove(t, "^ID ")
  res[[i]] <- str_split(t, "; ")
  # t <- str_replace(t, "[^;]$", " ")
  # t <- 
}

res <- res[which(!sapply(res, is.null))]
ky <- sapply(res, function(x)x[[1]])


ky_freq <- as.data.frame(table(unlist(ky)))%>% 
  arrange(desc(Freq))

term <- as.character(ky_freq$Var1)

res <- Matrix(0, nrow = length(term), ncol = length(term), dimnames = list(term, term))
for (i in 1:length(ky)){
  res[ky[[i]], ky[[i]]] <- 1 + res[ky[[i]], ky[[i]]]
}

write.table(ky_freq, "freq.txt", sep = "\t", quote = F, col.names = NA)

res2 <- res
res2 <- res2[as.character(ky_freq$Var1)[ky_freq$Freq>2], as.character(ky_freq$Var1)[ky_freq$Freq>2]]
summary(colSums(res2))

data.table::fwrite(as.data.frame(as.matrix(res2)),
                   file="关键词共现矩阵2.txt", 
                   quote = F, row.names = T, col.names = T, sep = "\t")
data.table::fwrite(as.data.frame(as.matrix(res)),
                   file="关键词共现矩阵全部.txt", 
                   quote = F, row.names = T, col.names = T, sep = "\t")

#高被引文献
CR <- which(str_detect(dat, "^CR "))
res2 <- list()
for(i in CR){
  tmp <- which(str_detect(dat[i:length(dat)], "^NR "))[1]
  t <- str_remove(dat[i:(i+tmp-2)], "^   ")
  t <- paste0(t, collapse = "; ")
  t <- str_remove(t, "^CR ")
  res2[[i]] <- str_split(t, "; ")
  # t <- str_replace(t, "[^;]$", " ")
  # t <- 
}

res2 <- res2[which(!sapply(res2, is.null))]
CR <- sapply(res2, function(x)x[[1]])

CR_freq <- as.data.frame(table(unlist(CR)), stringsAsFactors = F)%>% 
  arrange(desc(Freq))

CR_freq$author <- sapply(str_split(CR_freq$Var1, ", "), function(x)x[1])
CR_freq$pdat <- sapply(str_split(CR_freq$Var1, ", "), function(x)x[2])

CR_freq <- filter(CR_freq, Freq>3)
CR_freq$joural <- sapply(str_split(CR_freq$Var1, ", "), function(x)x[3])
CR_freq$DOI <- str_extract(CR_freq$Var1, "DOI .*$")
CR_freq$DOI <- str_remove(CR_freq$DOI, "^(DOI )+")
CR_freq$id <- str_remove(CR_freq$Var1, "DOI .*$")
CR_freq$id <- str_remove(CR_freq$id, ", $")

write.table(CR_freq, "CR_freq.txt", sep = "\t", quote = F, col.names = NA)

CR_freq <- as.data.frame(table(unlist(CR)), stringsAsFactors = F)%>% 
  arrange(desc(Freq))
term2 <- as.character(CR_freq$Var1)

res2 <- Matrix(0, nrow = length(term2), ncol = length(term2), dimnames = list(term2, term2))
for (i in 1:length(CR)){
  res2[CR[[i]], CR[[i]]] <- 1 + res2[CR[[i]], CR[[i]]]
}
res22 <- res2
res22 <- res22[as.character(CR_freq$Var1)[CR_freq$Freq>3], as.character(CR_freq$Var1)[CR_freq$Freq>3]]
summary(colSums(res22))

rownames(res22) <- CR_freq$id
colnames(res22) <- CR_freq$id

data.table::fwrite(as.data.frame(as.matrix(res22)),
                   file="高引文献共现矩阵3.txt", 
                   quote = F, row.names = T, col.names = T, sep = "\t")

#############
rm(list=ls())
gc()
options(encoding="utf-8")
dat <- readLines("twice/My EndNote Library.txt")
dat <- tolower(dat)
ky <- str_remove(dat[str_detect(dat, "key words")], "key words:")
ky <- ky[ky!="\t"]
ky <- ky[ky!=""]
ky <- ky[ky!=" "]
ky <- ky[ky!="-"]
ky <- str_remove(ky, "^ +")
ky <- str_remove(ky, "\\t")
ky <- str_remove(ky, "key words： ")

ky <- str_replace_all(ky , " +, +", ";")
ky <- str_replace_all(ky , ",", ";")
ky <- str_replace_all(ky , "， ", ";")
ky <- str_replace_all(ky , "，", ";")
ky <- str_remove(ky, "[^\\w]$")
ky[4] <- str_remove(ky[4], " policy position; ")

ky <- str_replace_all(ky , "organizational performance", "performance")
ky <- str_replace_all(ky , "anti-social behaviour", "behavior")
ky <- str_replace_all(ky , "bureaucrats|bureaucratic accountability|bureaucratic behavior|bureaucratic encounters|bureaucratic responsiveness", "bureaucrat")
ky <- str_replace_all(ky , "citizen participation|citizens' cognition", "citizen")
ky <- str_replace_all(ky , "cutbacks", "cutback management")
ky <- str_replace_all(ky , "street-level bureaucracy", "bureaucracy")
ky <- str_replace_all(ky , "coalition|collaboration|collaboration agreements", "collaborative governance")
ky <- str_replace_all(ky , "communtication", "communication")
ky <- str_replace_all(ky , "constitutional context", "context")
ky <- str_replace_all(ky , "contracting-out", "contracting")
ky <- str_replace_all(ky , "behaviour", "behavior")
ky <- str_replace_all(ky , "decision-making process|decisions|departure decisions", "decision-making")
# ky <- str_replace_all(ky , "bureaucrat", "bureaucracy")
ky <- str_replace_all(ky , "cognitive", "cognitive styles")
ky <- str_replace_all(ky , "public services", "public service")
# ky <- str_replace_all(ky , "bureaucrat", "bureaucracy")


ky <- str_split(ky, ";")
ky <- sapply(ky, function(x)str_remove(x, "^ +"))
ky <- sapply(ky, function(x)str_remove(x, " +$"))
ky <- sapply(ky, function(x)x[x!=""])
ky[[89]][1] <- "social housing"

ky_freq <- as.data.frame(table(unlist(ky)))%>% 
  arrange(desc(Freq))

term <- as.character(ky_freq$Var1)
sort(term[1:62])


res <- Matrix(0, nrow = length(term), ncol = length(term), dimnames = list(term, term))
for (i in 1:length(ky)){
  res[ky[[i]], ky[[i]]] <- 1 + res[ky[[i]], ky[[i]]]
}

write.table(ky_freq, "twice/freq.txt", sep = "\t", quote = F, col.names = NA)

res2 <- res
res2 <- res2[as.character(ky_freq$Var1)[1:61], as.character(ky_freq$Var1)[1:61]]
res2["bureaucrat", ][which(res2["bureaucrat", ]!=0)]
res2["bureaucracy", ][which(res2["bureaucracy", ]!=0)]
summary(colSums(res))
diag(res2) <- 0
summary(colSums(res2))
res2
data.table::fwrite(as.data.frame(as.matrix(res2)),
                   file="twice/关键词共现矩阵60.txt", 
                   quote = F, row.names = T, col.names = T, sep = "\t")
data.table::fwrite(as.data.frame(as.matrix(res)),
                   file="twice/关键词共现矩阵全部.txt", 
                   quote = F, row.names = T, col.names = T, sep = "\t")

res2 
