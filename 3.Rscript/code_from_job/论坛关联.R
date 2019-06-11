# Load libraries
library(Matrix)
library(dplyr)
library(stringr)

#Create Results directory
dir.create("./res", showWarnings = FALSE, recursive = T)

# set global options
options(stringsAsFactors = FALSE)
options(encoding="utf-8")

d1 <- readxl::read_xlsx("论坛参会人员信息汇总1108.xlsx")
sort(unique(d1$姓名))

d11 <- d1

d1$姓名 <- str_remove_all(d1$姓名, " ")
d1$姓名 <- str_remove_all(d1$姓名, "\r\nHanpingChen\r\n")
sort(unique(d1$姓名))

res1 <- group_by(d1, 姓名)%>% 
  select("姓名", "第几届")%>% 
  filter(!is.na(姓名))%>% 
  unique()%>% 
  mutate(number=n())%>%
  arrange(desc(number), 姓名)

write.table(res1, "res/人次统计.txt", col.names = T, row.names = F, quote = F, sep = "\t")

  # unique()
 #dplyr::distinct(姓名,第几届,na.rm = T)
 # group_by(第几届)%>%
  # arrange(姓名,第几届)%>%

res2 <- group_by(d1, 就职单位)%>% 
  select("就职单位", "第几届")%>% 
  filter(!is.na(就职单位))%>% 
  unique()%>% 
  mutate(number=n())%>%
  arrange(desc(number), 就职单位)

write.table(res2, "res/单位统计.txt", col.names = T, row.names = F, quote = F, sep = "\t")
  
# 数据2 
d21 <- readxl::read_xlsx("历届论坛主论坛、分论坛演讲嘉宾及演讲主题(1).xlsx", sheet=1)
d22 <- readxl::read_xlsx("历届论坛主论坛、分论坛演讲嘉宾及演讲主题(1).xlsx", sheet=2)
d23 <- readxl::read_xlsx("历届论坛主论坛、分论坛演讲嘉宾及演讲主题(1).xlsx", sheet=3)
d24 <- readxl::read_xlsx("历届论坛主论坛、分论坛演讲嘉宾及演讲主题(1).xlsx", sheet=4)
d25 <- readxl::read_xlsx("历届论坛主论坛、分论坛演讲嘉宾及演讲主题(1).xlsx", sheet=5)

# 背景分类:学校、政府、企业、NGO
zw1 <- d21$职务
zw1 <- str_replace(zw1, ".*大学.*", "学校")
zw1 <- str_replace(zw1, ".*市长.*", "政府")
zw1 <- str_replace(zw1, ".*处长.*", "政府")
zw1 <- str_replace(zw1, ".*世界银行.*", "政府")
zw1 <- str_replace(zw1, ".*厅长.*", "政府")
zw1 <- str_replace(zw1, ".*香港绿色建筑议会.*", "NGO")
zw1 <- str_replace(zw1, ".*公司.*", "企业")
zw1 <- str_replace(zw1, ".*高雄市建筑师公会.*", "NGO")
zw1 <- str_replace(zw1, ".*市委书记.*", "政府")
zw1 <- str_replace(zw1, ".*学院.*", "学校")
zw1 <- str_replace(zw1, ".*教授.*", "学校")
zw1 <- str_replace(zw1, ".*气候办.*", "政府")
zw1 <- str_replace(zw1, ".*瑞典斯堪尼亚中国战略中心.*", "企业")
zw1 <- str_replace(zw1, ".*英国废物管理特许机构.*", "政府")
zw1 <- str_replace(zw1, ".*深圳市建筑科学研究院.*", "企业")
zw1 <- str_replace(zw1, ".*北京城市规划设计研究院.*", "政府")
zw1 <- str_replace(zw1, ".*中国城市科学研究会.*", "政府")
zw1 <- str_replace(zw1, ".*政府.*", "政府")
zw1 <- str_replace(zw1, ".*国务院.*", "政府")
zw1 <- str_replace(zw1, ".*市规划.*", "政府")
zw1 <- str_replace(zw1, ".*中国社会科学院.*", "学校")
zw1 <- str_replace(zw1, ".*英国奥雅纳工程.*", "企业")
zw1 <- str_replace(zw1, ".*英国奥雅纳工程.*", "企业")
zw1 <- str_replace(zw1, ".*英国奥雅纳工程.*", "企业")
zw1 <- str_replace(zw1, ".*英国奥雅纳工程.*", "企业")
zw1
