#Reference for code:
#https://www.r-bloggers.com/twitter-sentiment-analysis-with-r/
library(twitteR)
library(RCurl)
library(stringr)
library(tm)
library(SnowballC)
library(ROAuth)

#rm(list = ls())
#setwd("/Users/nehamansinghka/Dropbox/Documents/File/MS Bus Analytics/Stevens/Program Content/BIA658 Social Network Analysis/Rong Duan WebCampus Class/Project")

# Declare Twitter API Credentials
api_key <- "WWWWWWWWWW" # From dev.twitter.com
api_secret <- "ZZZZZZZZZZZZZZZZ" # From dev.twitter.com
token <- "YYYYYYYYYY" # From dev.twitter.com
token_secret <- "XXXXXXXXXXX" # From dev.twitter.com

# Create Twitter Connection
setup_twitter_oauth(api_key, api_secret, token, token_secret)

search <- function(searchterm)
{
  #access tweets and create cumulative file
  list <- searchTwitter(searchterm, resultType="recent", n=180)
  df <- twListToDF(list)
  df <- df[, order(names(df))]
  df$created <- strftime(df$created, '%Y-%m-%d')
  if (file.exists(paste(searchterm, '_stack.csv'))==FALSE) write.csv(df, file=paste(searchterm, '_stack.csv'), row.names=F)
  #merge last access with cumulative file and remove duplicates
  stack <- read.csv(file=paste(searchterm, '_stack.csv'))
  stack <- rbind(stack, df)
  stack <- subset(stack, !duplicated(stack$text))
  write.csv(stack, file=paste(searchterm, '_stack.csv'), row.names=F)
}

