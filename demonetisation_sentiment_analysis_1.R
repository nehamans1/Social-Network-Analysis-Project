#References for the code:
#http://www.slideshare.net/ajayohri/twitter-analysis-by-kaify-rais
#https://github.com/arathee2/demonetization-india/blob/master/demonetization-sentiment-analysis.md
#https://www.r-bloggers.com/twitter-sentiment-analysis-with-r/
library(twitteR)
library(tm)
library(SnowballC)
library(wordcloud)
library(RCurl)
library(ROAuth)
library(stringr)
library(ggplot2)
library(plyr)
library(dplyr)

demonetweets.df <- read.csv("demonetization-old-tweets.csv")
#demonetweets.df <- read.csv("demonetization_stack_copy.csv")
str(demonetweets.df)

tweets <- as.character(demonetweets.df$text)

sentiment.score <- function(sentences, positive.words, negative.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we have a vector of sentences. plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  scores <- laply(sentences, function(sentence, positive.words, negative.words)
  {
    #remove emoticons
    sentence <- iconv(sentence, "latin1", "ASCII", sub="")
    
    ## clean up sentences with R's regex-driven global substitute, gsub():
    
    sentence <- gsub('[[:cntrl:]]', '', sentence)
    
    # remove retweets
    sentence <- gsub('(RT|via)((?:\\b\\W*@\\W+)+)', '', sentence)
    
    # remove at people
    sentence <- gsub('@\\w+', '', sentence)
    
    # remove punctuations
    sentence <- gsub('[[:punct:]]', '', sentence)
    
    # remove numbers
    sentence <- gsub('[[:digit:]]', '', sentence)
    
    # remove html links
    sentence <- gsub('http[s]?\\w+', '', sentence)
    
    # remove extra spaces
    sentence <- gsub('[ \t]{2,}', '', sentence)
    sentence <- gsub('^\\s+|\\s+$', '', sentence)
    
    # removing NA's
    sentence <- sentence[!is.na(sentence)]
    
    # convert to lower case:
    sentence <- tolower(sentence)
    
    # split into words. str_split is in the stringr package
    
    word.list <- str_split(sentence, '\\s+')
    
    # sometimes a list() is one level of hierarchy too much
    
    words <- unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    
    negative.matches <- match(words, negative.words)
    positive.matches <- match(words, positive.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    
    positive.matches <- !is.na(positive.matches)
    negative.matches <- !is.na(negative.matches)
    
    # and TRUE/FALSE will be treated as 1/0 by sum():
    
    score <- sum(positive.matches) - sum(negative.matches)
    
    return(score)
  }, positive.words, negative.words, .progress=.progress )
  
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}

positive <- scan("positive_words.txt", what= "character", comment.char= ";")
negative <- scan("negative_words.txt", what= "character", comment.char= ";")

demonetweets.analysis <- sentiment.score(tweets, positive, negative, .progress="none")

#write.csv(demonetweets.analysis, file='New_File.csv', row.names=TRUE)

stat <- demonetweets.analysis
stat$created <- demonetweets.df$created
stat$created <- as.Date(stat$created)
stat <- mutate(stat, tweet=ifelse(stat$score > 0, 'positive', ifelse(stat$score < 0, 'negative', 'neutral')))
by.tweet <- group_by(stat, tweet, created)
by.tweet <- summarise(by.tweet, number=n())
#write.csv(by.tweet, file=paste(searchterm, '_opin.csv'), row.names=TRUE)
#create chart
ggplot(by.tweet, aes(created, number)) + geom_line(aes(group=tweet, color=tweet), size=2) +
  geom_point(aes(group=tweet, color=tweet), size=4) +
  theme(text = element_text(size=18), axis.text.x = element_text(angle=90, vjust=1))
  