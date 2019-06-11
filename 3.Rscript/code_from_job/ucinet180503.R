# set global options
options(stringsAsFactors = FALSE)

# Load libraries
library(dplyr)
library(stringr)

ego <- as.data.frame(readxl::read_xlsx("Ego Mastersheet.xlsx", sheet = 1))

ego[1:5, 1:3]

#tobacco smoke gambling Gamble
filter(ego, 'AUDIT TOTAL'>=8 & 'AUDIT TOTAL'<=15 & Smoke == 1 & Marijuana ==1 & Gamble ==1)%>% 
  select(Subject, starts_with("AUDIL"), Smoke, Marijuana, Gamble)

# min 10
summary(ego$`AUDIT TOTAL`)
table(ego$Smoke)
table(ego$Marijuana)
table(ego$Gamble)

ego1 <- filter(ego, between(ego$`AUDIT TOTAL`, 8, 15)& Smoke == 1 & Marijuana ==1 & Gamble ==1)%>% 
  select(Subject, starts_with("AUDIT TOTAL"), Smoke, Marijuana, Gamble)

ego2 <- filter(ego, between(ego$`AUDIT TOTAL`, 16, 20)& Smoke == 2 & Marijuana ==3 & Gamble ==1)%>% 
  select(Subject, starts_with("AUDIT TOTAL"), Smoke, Marijuana, Gamble)

ego3 <- filter(ego, ego$'AUDIT TOTAL'>20 & Smoke == 2 & Marijuana ==2 & Gamble ==1)%>% 
  select(Subject, starts_with("AUDIT TOTAL"), Smoke, Marijuana, Gamble)

ego4 <- filter(ego, ego$'AUDIT DEPENDENCE' < 4 & Smoke == 1 & Marijuana ==2 & Gamble ==1)%>% 
  select(Subject, starts_with("AUDIT DEPENDENCE"), Smoke, Marijuana, Gamble)

ego5 <- filter(ego, between(ego$`AUDIT DEPENDENCE`, 4, 12) & Smoke == 2 & Marijuana ==3 & Gamble ==1)%>% 
  select(Subject, starts_with("AUDIT DEPENDENCE"), Smoke, Marijuana, Gamble)

# ego1:4 ego2:265 ego3:228 ego4:46 ego5:265
alter_adj <- as.data.frame(readxl::read_xlsx("Weighted Adjacency Mastersheet.xlsx", sheet = 1))
alter_attr <- as.data.frame(readxl::read_xlsx("Alter Summary Mastersheet.xlsx", sheet = 1))

ADJ <- function(x,y){
  m1 <- filter(alter_adj, Part==x)%>% 
    select(starts_with("Alter"))
  m1 <- m1[,-1]
  rownames(m1) <- colnames(m1)
  write.table(m1, paste0("Adja_ego_",y,".txt"), col.names = NA, quote = F, sep = "\t")
  
  t1 <- filter(alter_attr, Participant==x)%>% 
    select(Relationship, Gamble, Tobacco,  Marijuana, Alcohol)
  rownames(t1) <- rownames(m1)
  write.table(t1, paste0("Attr_ego_",y,".txt"), col.names = NA, quote = F, sep = "\t")
  
  return(m1)
}

m1 <- ADJ(4, 1)
m2 <- ADJ(265, 2)
m3 <- ADJ(228, 3)
m4 <- ADJ(46, 4)
m5 <- ADJ(265, 5)

library(igraph)
?walktrap.community()
library(MCL)
mcl(x = m4, addLoops=TRUE, ESM = TRUE)
# library(igraph)
gu <- graph.adjacency( adjacency, mode="undirected" )
plot( gu )

mz <- matrix(0, 150, 150)
mz[1:30,]