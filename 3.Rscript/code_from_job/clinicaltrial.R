# Load libraries
library(XML)
library(xml2)
library(rvest)
library(dplyr)
library(stringr)


#Create Results directory
dir.create("./res", showWarnings = FALSE, recursive = T)

# set global options
options(stringsAsFactors = FALSE)
options(encoding="utf-8")

NCT <- read.delim("nct.txt")
NCT <- as.vector(t(NCT))

clinical_trial <- function(nctid){
  gurl <- paste0("https://clinicaltrials.gov/ct2/results?cond=&term=", nctid)
  ct1 <- gurl %>% read_html(encoding="utf-8")
  t1 <- ct1 %>% html_nodes(xpath="//table")
  # t1 <- ct1%>% html_table(header = T)
  # t1 <- t1[[2]]
  # t1 <- t1[c(2,4,)]
  # ifelse(length(html_nodes(t1,xpath = "./tbody/tr/td[5]/ul/li"))==0, html_nodes(t1,xpath = "./tbody/tr/td[5]")%>% 
  #          html_text,  paste(html_nodes(t1,xpath = "./tbody/tr/td[5]/ul/li")%>% html_text, collapse = ";"))
  
  index <- c(2,4,5,3,7,19,21,9,25)
  tmp <- c()
  for(i in index){
    if(length(html_nodes(t1,xpath = paste0("./tbody/tr/td[",i,"]/ul/li")))==0){
      tmp <- c(tmp,html_nodes(t1,xpath = paste0("./tbody/tr/td[",i,"]"))%>% html_text)
    }else{
      tmp <- c(tmp,paste(html_nodes(t1,xpath = paste0("./tbody/tr/td[",i,"]/ul/li"))%>% html_text, collapse = ";"))
    }
  }
  
  # locations <- strsplit(as.character(html_nodes(t1,xpath = paste0("./tbody/tr/td[",25,"]/ul/li"))), "<br>")
  # loca1 <- 
  
  Sys.sleep(0.5)
  
  return(tmp)
}

# clinical_trial(NCT[200])

res <- sapply(NCT[1:100], clinical_trial)
res2 <- sapply(NCT[101:150], clinical_trial)
res22 <- sapply(NCT[151:160], clinical_trial)
res2222 <- sapply(NCT[161:170], clinical_trial)
res222 <- sapply(NCT[171:200], clinical_trial)
res3 <- sapply(NCT[201:250], clinical_trial)
res33 <- sapply(NCT[251:300], clinical_trial)
res4 <- sapply(NCT[301:330], clinical_trial)
res42 <- sapply(NCT[331:340], clinical_trial)
res423 <- sapply(NCT[c(341:344)], clinical_trial)#345
res4441 <- c(NCT[345], rep("", 8))#347:349
res4442 <- sapply(NCT[346], clinical_trial)
res4443 <- c(NCT[347], rep("", 8))#347:349
res4444 <- c(NCT[348], rep("", 8))#347:349
res4445 <- sapply(NCT[349:350], clinical_trial)
res44 <- sapply(NCT[351:400], clinical_trial)
res5 <- sapply(NCT[401:450], clinical_trial)
res6 <- sapply(NCT[451:515], clinical_trial)

res44 <- sapply(res44[-26], function(x)x)
res <- sapply(res[-92], function(x)x)

resz1 <- c(NCT[92], rep("", 8))
resz2 <- c(NCT[376], rep("", 8))
resz3 <- c(NCT[516], rep("", 8))

RES <- cbind(res[,1:91], resz1, res[,92:99], res2, res22, res2222, res222, 
             res3, res33, res4, res42, res423, res4441, 
             res4442, res4443, res4444, res4445, res44[,1:25], resz2, res44[,26:49], res5, res6, resz3)
rownames(RES) <- c("nctid", "title", "conditions", "status", "study_type", "start_time", "end_time", "sponsor/collaborators", "locations")
RES <- data.frame(t(RES))
# rownames(RES) <- RES$nctid
# RES2 <- data.frame(nctid=NCT[is.na(match(RES2$nctid, RES$nctid))])

identical(NCT, RES$nctid)

RES[c(92,376,345,347:348,516),]
RES[376,] <- clinical_trial2(NCT[376])
RES[92,] <- clinical_trial2(NCT[92])
RES[516,] <- clinical_trial2("NCT02480764")

clinical_trial2 <- function(nctid){
  gurl <- paste0("https://clinicaltrials.gov/ct2/results?cond=&term=", NCT[376])
  ct1 <- gurl %>% read_html(encoding="utf-8")
  t1 <- ct1 %>% html_nodes(xpath="//table")
  # t1 <- ct1%>% html_table(header = T)
  # t1 <- t1[[2]]
  # t1 <- t1[c(2,4,)]
  # ifelse(length(html_nodes(t1,xpath = "./tbody/tr/td[5]/ul/li"))==0, html_nodes(t1,xpath = "./tbody/tr/td[5]")%>% 
  #          html_text,  paste(html_nodes(t1,xpath = "./tbody/tr/td[5]/ul/li")%>% html_text, collapse = ";"))
  
  index <- c(2,4,5,3,7,19,21,9,25)
  tmp <- c()
  for(i in index){
    if(length(html_nodes(t1,xpath = paste0("./tbody/tr[1]/td[",i,"]/ul/li")))==0){
      tmp <- c(tmp,html_nodes(t1,xpath = paste0("./tbody/tr[1]/td[",i,"]"))%>% html_text)
    }else{
      tmp <- c(tmp,paste(html_nodes(t1,xpath = paste0("./tbody/tr[1]/td[",i,"]/ul/li"))%>% html_text, collapse = ";"))
    }
  }
  
  # locations <- strsplit(as.character(html_nodes(t1,xpath = paste0("./tbody/tr/td[",25,"]/ul/li"))), "<br>")
  # loca1 <- 
  
  Sys.sleep(0.5)
  
  return(tmp)
}

#修改时间格式
lct <- Sys.getlocale("LC_TIME")
lct
Sys.setlocale("LC_TIME", "C")
# Sys.setlocale("LC_TIME", "Chinese (Simplified)_People's Republic of China.936")

as.Date(RES$start_time[1], "%B %Y") 

RES$start_time[str_detect(RES$start_time, "\\w+ \\d+$")] <- 
  as.character(as.Date(paste0(RES$start_time[str_detect(RES$start_time, "\\w+ \\d+$")], "01"), "%B %Y%d"))

RES$start_time[str_detect(RES$start_time, "\\w+ \\d+, \\d+$")] <- 
  as.character(as.Date(paste0(RES$start_time[str_detect(RES$start_time, "\\w+ \\d+, \\d+$")], "01"), "%B %d, %Y"))

RES$start_time <- str_replace_all(RES$start_time, "-", "/")
RES2 <- RES#备份
# RES <- RES2
RES$end_time[str_detect(RES$end_time, "\\w+ \\d+$")] <- 
  as.character(as.Date(paste0(RES$end_time[str_detect(RES$end_time, "\\w+ \\d+$")], "01"), "%B %Y%d"))

RES$end_time[str_detect(RES$end_time, "\\w+ \\d+, \\d+$")] <- 
  as.character(as.Date(paste0(RES$end_time[str_detect(RES$end_time, "\\w+ \\d+, \\d+$")], "01"), "%B %d, %Y"))

RES$end_time <- str_replace_all(RES$end_time, "-", "/")

Sys.setlocale("LC_TIME", lct)

RES$nctid[str_detect(RES$conditions, "more")]
RES$conditions[str_detect(RES$conditions, "more")]
index_c <- which(str_detect(RES$conditions, "more"))
RES$conditions[index_c[1]] <- paste(str_remove(RES$conditions[index_c[1]], ";\\(.*more.*$"), 
                                    "Conbercept", "Pharmacogenomic", sep = ";")

RES$conditions[index_c[2]] <- paste(str_remove(RES$conditions[index_c[2]], ";\\(.*more.*$"), 
                                    "Locally Advanced Cancer", "Chemoradiation", sep = ";")


unique(RES$status)
RES$status[RES$status==unique(RES$status)[8]] <- unique(RES$status)[5]
RES$status[RES$status==unique(RES$status)[8]] <- unique(RES$status)[2]

unique(RES$study_type)


# sponsor
sc <- str_split(RES$sponsor.collaborators, ";", n = 2) 
sponsor <- sapply(sc, function(x)x[1])
collaborators <- sapply(sc, function(x)paste(x[-1], collapse = ";"))

RES3 <- cbind(RES, sponsor, collaborators)


RES$locations[1:20]

RES3[RES3==""] <- "缺失" 

write.table(RES3, "res/RES3.txt", sep = "\t", quote = F, col.names = T, row.names = F)


