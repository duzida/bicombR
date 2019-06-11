# Install and Load libraries
# installed.packages(c("dplyr","igraph","Matrix"))
library(dplyr)
library(rjson)
library(stringr)
# library(reshape2)

# Create 01_Results directory
# dir.create("./res", showWarnings = FALSE)

# set global options
options(stringsAsFactors = FALSE)
# options(useFancyQuotes = FALSE)

log <- readLines("8989.01161.log")
# log[3]
res <- c()
for(i in 1:length(log)){
  log[i] <- str_to_lower(log[i])
  tmp <- stringr::str_detect(log[i], c("fromusername","sendernickname","groupname",
                                "msgtype","tousername"))
  if(any(tmp)){
   res <- c(res,i) 
  }
  }

res2 <- log[res]

for(i in 1:length(res2)){
  # res2[i] <- str_replace(res2[i], "^\\d+.*\\[\\w+\\]", "")
  res2[i] <- str_extract(res2[i], "\\{.*\\}")
}

unique(str_sub(res2,1,1))
which(str_sub(res2,1,1)!="{")

which(str_detect(res2, "sendernickname"))
# res3 <- fromJSON(res2[1:5])
r3 <- sapply(1:length(res2), function(x){
  x1 <- tryCatch(fromJSON(res2[x]),error=function(e){cat(x," ", conditionMessage(e), "\n")})
  if(is.null(x1)){
    return(c(NA,NA,NA,NA,NA))
  }
  fromusername <- x1$fromusername
  if(length(fromusername)==0){
    fromusername <- NA
  }
  tousername <- x1$tousername
  if(length(tousername)==0){
    tousername <- NA
  }
  msgtype <- x1$msgtype
  if(length(msgtype)==0){
    msgtype <- NA
  }
  sendernickname <- x1$sendernickname
  if(length(sendernickname)==0){
    sendernickname <- NA
  }
  groupname <- x1$groupname
  if(length(groupname)==0){
    groupname <- NA
  }
  xx <- c(fromusername, tousername, msgtype, sendernickname, groupname)
})
names(r3) <- NULL
r3 <- t(r3) 
colnames(r3) <- c("fromusername","tousername","msgtype",
                   "sendernickname","groupname")
r3 <- r3[!apply(r3,1,function(x)all(is.na(x))),]
write.table(r3, "8989.01161.txt", col.names = T, row.names = F, 
            quote = F, sep = "\t")
Encoding(r3[13,2])
r3[13,]
