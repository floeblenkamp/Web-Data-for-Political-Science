#
# File No 1: Data Scraping
# 
library(rtweet)
library(ggplot2)
library(dplyr)
library(tidytext)
library(streamR)
library(ROAuth)

# Part 1: Scrape Twitter Data for network analysis ----
## load twitter database: sheet with all usernames of Nationalrat/St?nderat MPs----
twitter <- read.csv("twitter.csv", header= TRUE)
twitter <- twitter %>% filter(institution == "sr" )
# pages == "svp" | pages == "sp" | pages == "glp" |  pages == "gruene" |
#  pages == "mitte" | pages == "fdp" | party == "BR"|

# load API Data
load("api_twitter.Rdata")

twitter$usernames
## scrape twitter ids of MPs----
tid <- vector()
for (i in twitter$usernames){
  id <-  search_users(i, n = 3, token = twitter_token)[1,1] %>% as.character()
  tid <- append(id, tid)
}
twitter$id <- rev(tid)



## scrape absolute number of followers for MPs ----
likes <- vector()
for (k in twitter$usernames){
  l <- search_users(k, n = 1, token = twitter_token)[1,78] %>% as.numeric()
  likes <- append(l, likes) 
}
twitter$likes <- rev(likes[1:nrow(twitter)])
#write.csv(twitter, "twitter.csv")

## scrape twitter followers---- 

df <- as.data.frame(matrix(NA,5000,1))
for (i in twitter$id){
  print(i)
  fol <- get_followers(user = i, n = 100000, token = twitter_token, retryonratelimit = TRUE) #%>% `colnames<-`(colnames(df))
  df <- qpcR:::cbind.na(fol, df)
  write.csv(df, "df.csv")
  df<- read.csv("df.csv", row.names = 1, header= TRUE )
}
df <- df[,1:nrow(twitter)]

colnames(df) <- rev(twitter$pages)
write.csv(df, "df_sr.csv")

######################################################

# Second part: analyse the network of twitter users ----
## identify unique users ----
fol_mat <- read.csv("df_sr.csv", row.names = 1, header= TRUE)
count <- table(unlist(fol_mat)) %>% as.data.frame() %>% filter(Freq > 2) #these users follow at least two MPs. 
users <- count$Var1

## who follows who: association matrix ----
matrix <- matrix(NA,length(users),ncol(fol_mat)) %>% 
  `colnames<-`(colnames(fol_mat)) %>% `rownames<-`(users)
# see you tomorrow
for (j in 1:ncol(fol_mat)){
  for (i in 1:length(users)){
    matrix[i,j] <- users[i] %in% fol_mat[,j]%>% as.integer()
    print(j)
  }}

matrix <- as.data.frame(t(matrix))
write.csv(matrix, file = "user-pol-matrix_sr.csv")

########################### 

# Part three measure the Ideology of Mps based on their twitter users ----
rm(list=ls())

library(rtweet)
library(ggplot2)
library(dplyr)
library(tidytext)
library(streamR)
library(ROAuth)
#library(shallot)
#library(vegan)

setwd("C:/Users/HP/OneDrive/UZH/Sommersemester2021/Forschungsseminar Datenjournalismus/twitter")


users <- read.csv("user-pol-matrix_sr.csv", row.names = 1, header= TRUE)
users[1:3,1:3]

twitter <- read.csv("twitter.csv", row.names = 1, header= TRUE) %>% filter(institution == "sr")
#twitter <- twitter %>% filter(pages == "svp" | pages == "sp" | pages == "glp" |  pages == "gruene" |
#                                pages == "mitte" | pages == "fdp" | party == "BR")

twitter <- twitter %>% na.omit

## get absolute combined likes per page ####
users <- as.data.frame(t(users))
head(users) # needs to be in long format
ma <- vector ()
for (j in colnames(users)){
  for (i in colnames(users)){
    ass <- users %>% filter(!!as.symbol(j) == 1 & !!as.symbol(i) == 1) %>% nrow()%>%as.numeric()
    print(ass)
    ma <- append(ass, ma)
  }}

ma <- rev(ma)
chunklength <- nrow(twitter)
matrix <- split(ma, ceiling(seq_along(ma)/chunklength)) %>% as.data.frame() %>% `colnames<-`(twitter$pages) %>%
  `rownames<-`(twitter$pages)

## diagonals of matrix: absolute likes per page ----
for (i in 1:nrow(twitter)){
  matrix[i,i] <- twitter$likes[i]
}


write.csv(matrix, "matrix_sr.csv")

## ratio of affiliation matrix ----

matrix <- read.csv("matrix_sr.csv", row.names = 1, header= TRUE)


ratiomat <- matrix(nrow = nrow(twitter), ncol = nrow(twitter), data = NA) %>% as.data.frame() %>% `colnames<-`(twitter$pages) %>%
  `rownames<-`(twitter$pages)

for (i in 1:nrow(twitter)){
  for (j in 1:nrow(twitter)){
    ratiomat[j,i] <- matrix[j,i]/matrix[j,j] %>% round(., digits = 3)
  }} 

write.csv(ratiomat, "ratiomat_sr.csv") # ratiomat.sr is available as a csv file in the data repository
