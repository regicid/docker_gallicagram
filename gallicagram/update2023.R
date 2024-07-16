library(rvest)
library(stringr)
setwd("~/docker_gallicagram/gallicagram")
a = read_html("https://www.nytimes.com/search?dropmab=false&query=have&endDate=20231231&startDate=20230101&types=article")
count = str_split(html_text(a),'totalCount":')[[1]][2]
count = str_split(count,",")[[1]][1]
system("head -n -1 base_presse_annees_nyt.csv > temp.csv") ##Delete last line
system(str_c("echo 173,",count, ",2023 >> temp.csv"))
system("mv temp.csv base_presse_annees_nyt.csv")
#system("bash ~/executor")
