# hello-world
this is the code that i used to write for my final year poject about classifying youth emotion on kpop and kdrama from different type of country which are Malaysia and Korea and the software that i used was R Studio.

install.packages("twitteR")
install.packages("igraph")
install.packages("flexdashboard")


library(twitteR)
library(igraph)


setwd("D:/trend")
getwd()

if (!require('pacman')) install.packages('pacman&')
pacman::p_load(devtools, installr)
install.Rtools()
install_url('http://cran.r-project.org/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz')
install_url('http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz')

if (!require('pacman')) install.packages('pacman')
pacman::p_load(twitteR, sentiment, plyr, ggplot2, wordcloud, RColorBrewer, httpuv, RCurl, base64enc)

options(RCurlOptions = list(cainfo = system.file('CurlSSL', 'cacert.pem', package = 'RCurl')))


api_key<-"YBBYi4xUeo4kz66Sc6beE6epr"
api_secret<-"GVI6ZTCPyTN4PcBI2EzuY7Pk8uEaxkjGOMhK6Sr6f4Tezw5S8T"
access_token<-"137970027-PhKMpeopIAndJlirnqQ5oAir1SBblVHOPnp8JRhK"
access_token_secret<-"gMJdBbKwmJEvwIbkEjW8E4YKgMQcV8WoUIkwlh09X9G8G"
twittersetup<-setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

1
#Load the necessary package
#source('authenticate.R')


Kpop_tweets = searchTwitter("Kpop", n=3000, lang="en", geocode ='4.210484,101.97576600000002, 100mi')
Kdrama_tweets = searchTwitter("Kdrama", n=3000, lang="en", geocode ='4.210484,101.97576600000002, 100mi')
KpopKR_tweets = searchTwitter("kpop", n=3000, lang="en", geocode ='35.907757,127.76692200000002, 100mi')
KdramaKR_tweets = searchTwitter("kdrama", n=3000, lang="en", geocode ='35.907757,127.76692200000002, 100mi')

length(Kpop_tweets)
length(KpopKR_tweets)
length(Kdrama_tweets)
length(KdramaKR_tweets)

head(Kpop_tweets)
head(Kdrama_tweets)
head(KpopKR_tweets)
head(KdramaKR_tweets)

KpopTweets <- sapply(Kpop_tweets, function(x) x$getText())
KdramaTweets = sapply(Kdrama_tweets, function(x) x$getText())
KpopKRTweets <- sapply(KpopKR_tweets, function(x) x$getText())
KdramaKRTweets = sapply(KdramaKR_tweets, function(x) x$getText())

catch.error = function(x)
{
  #create missing value
  y=NA
  #try to catch error (NA)
  catch_error = tryCatch(tolower(x), error=function(e) e)
  #if not an error
  if(!inherits(catch_error,"error"))
    y=tolower(x)
  #check result if error exists, otherwise the function work fine
  return(y)
}

cleanTweets <- function(tweet)
{
  #clean the tweets for sentiment analysis
  #remove html links, which are not required for sentiment analysis
  tweet = gsub ("(f|ht)(tp)(s?)(://)(.*)"," ", tweet)
  #remove retweets entities from the stored tweets (text)
  tweet = gsub ("(RT|via) ((?:\\b\\W*@\\w+)+)", " ", tweet)
  #remove all the #HASHTAG
  tweet = gsub("#\\w+", " ", tweet)
  #remove all @people
  tweet = gsub("@\\w+", " ", tweet)
  #remove punctuation
  tweet = gsub ("[[:punct:]]", " ", tweet)
  #remove numbers
  tweet = gsub ("[[:digit:]]", " ", tweet)
  #remove unnecessary spaces (white spaces, tabs etc)
  tweet = gsub ("[ \t] {2,}", " ", tweet)
  tweet = gsub ("^\\s+|\\s+$", " ", tweet)
  tweet = catch.error(tweet)
  tweet
}


cleanTweetsAndRemoveNAs <- function(Tweets)
{
  TweetsCleaned = sapply(Tweets, cleanTweets)
  #remove the "NA" tweets from this list
  TweetsCleaned = TweetsCleaned[!is.na(TweetsCleaned)]
  names(TweetsCleaned) = NULL
  #remove the repetitive tweet from tweet list
  TweetsCleaned = unique(TweetsCleaned)
  TweetsCleaned
}

KpopTweetsCleaned = cleanTweetsAndRemoveNAs(KpopTweets)
KpopKRTweetsCleaned = cleanTweetsAndRemoveNAs(KpopKRTweets)
KdramaTweetsCleaned = cleanTweetsAndRemoveNAs(KdramaTweets)
KdramaKRTweetsCleaned = cleanTweetsAndRemoveNAs(KdramaKRTweets)

length(KpopTweetsCleaned)
length(KpopKRTweetsCleaned)
length(KdramaTweetsCleaned)
length(KdramaKRTweetsCleaned)

head(KpopTweetsCleaned)
head(KpopKRTweetsCleaned)
head(KdramaTweetsCleaned)
head(KdramaKRTweetsCleaned)

library(sentiment)

#classify_emotion function return and object of class data frame
#with seven columns (anger, disgust, fear, joy, sadness, surprise # # best_fit)
#and one row for each document:
KpopTweetsClassEmo = classify_emotion(KpopTweetsCleaned, algorithm="bayes", prior=1.0)
KdramaTweetsClassEmo = classify_emotion(KdramaTweetsCleaned, algorithm="bayes", prior=1.0)
KpopKRTweetsClassEmo = classify_emotion(KpopKRTweetsCleaned, algorithm="bayes", prior=1.0)
KdramaKRTweetsClassEmo = classify_emotion(KdramaKRTweetsCleaned, algorithm="bayes", prior=1.0)

#fetch emotion category best_fit for analysis purposes
KpopEmotion = KpopTweetsClassEmo [,7]
KpopKREmotion = KpopKRTweetsClassEmo [,7]
KdramaEmotion = KdramaTweetsClassEmo [,7]
KdramaKREmotion = KdramaKRTweetsClassEmo [,7]

#KpopEmotion[is.na(KpopEmotion)] =  "neutral"
#KdramaEmotion[is.na(KdramaEmotion)] =  "neutral"


KpopTweetsClassPol = classify_polarity(KpopTweetsCleaned, algorithm="bayes")
KpopKRTweetsClassPol = classify_polarity(KpopKRTweetsCleaned, algorithm="bayes")
KdramaKRTweetsClassPol = classify_polarity(KdramaKRTweetsCleaned, algorithm="bayes")
KdramaTweetsClassPol = classify_polarity(KdramaTweetsCleaned, algorithm="bayes")
#plotting.. for analysis purposes

KpopPol = KpopTweetsClassPol[,4]
KdramaPol = KdramaTweetsClassPol[,4]
KpopKRPol = KpopKRTweetsClassPol[,4]
KdramaKRPol = KdramaKRTweetsClassPol[,4]

#create dataframe with above result
KpopSentimentDataFrame = data.frame(text=KpopTweetsCleaned, Emotion=KpopEmotion, 
                                    polarity=KpopPol, stringsAsFactors = FALSE)
KdramaSentimentDataFrame = data.frame(text=KdramaTweetsCleaned, Emotion=KdramaEmotion, 
                                      polarity=KdramaPol, stringsAsFactors = FALSE)
KpopKRSentimentDataFrame = data.frame(text=KpopKRTweetsCleaned, Emotion=KpopKREmotion, 
                                    polarity=KpopKRPol, stringsAsFactors = FALSE)
KdramaKRSentimentDataFrame = data.frame(text=KdramaKRTweetsCleaned, Emotion=KdramaKREmotion, 
                                      polarity=KdramaKRPol, stringsAsFactors = FALSE)
#rearerange data inside the frame by sorting it

KpopSentimentDataFrame = within(KpopSentimentDataFrame, Emotion <- factor(Emotion,
                                                                          levels = names(sort(table(Emotion), decreasing = TRUE))))
KdramaSentimentDataFrame = within(KdramaSentimentDataFrame, Emotion <- factor(Emotion,
                                                                              levels = names(sort(table(Emotion), decreasing = TRUE))))
KpopKRSentimentDataFrame = within(KpopKRSentimentDataFrame, Emotion <- factor(Emotion,
                                                                          levels = names(sort(table(Emotion), decreasing = TRUE))))
KdramaKRSentimentDataFrame = within(KdramaKRSentimentDataFrame, Emotion <- factor(Emotion,
                                                                              levels = names(sort(table(Emotion), decreasing = TRUE))))

plotSentiment1 <- function(sentiment_dataframe, title){
  library(ggplot2)
  ggplot(sentiment_dataframe, aes(x=Emotion)) +
    geom_bar(aes(y=..count.., fill=Emotion)) +
    scale_fill_brewer(palette = "Dark2") + ggtitle(title) +
    theme(legend.position = 'right') + ylab('Number of Tweets') + xlab ('Emotion Categories')
}

plotSentiment1(KpopSentimentDataFrame, 'Sentiment Analysis of Tweets on Twitter about Kpop in Malaysia')
plotSentiment1(KpopKRSentimentDataFrame, 'Sentiment Analysis of Tweets on Twitter about Kpop in Korea')
plotSentiment1(KdramaSentimentDataFrame, 'Sentiment Analysis of Tweets on Twitter about Kdrama in Malaysia')
plotSentiment1(KdramaKRSentimentDataFrame, 'Sentiment Analysis of Tweets on Twitter about Kdrama in Korea')
#plot distribution of polarity in the tweets 
plotSentiment2 <- function(sentiment_dataframe,title){
  library(ggplot2)
  ggplot(sentiment_dataframe, aes(x=polarity)) +
    geom_bar(aes(y=..count.., fill=polarity)) +
    scale_fill_brewer(palette = "RdGy") +
    ggtitle(title)+
    theme(legend.position = 'right') + ylab('Number of Tweets') + xlab ('Polarity categories')
}

plotSentiment2(KpopSentimentDataFrame, 'Polarity Analysis of Tweets on Twitter about Kpop in Malaysia')
plotSentiment2(KpopKRSentimentDataFrame, 'Polarity Analysis of Tweets on Twitter about Kpop in Korea')
plotSentiment2(KdramaSentimentDataFrame, 'Polarity Analysis of Tweets on Twitter about Kdrama in Malaysia')
plotSentiment2(KdramaKRSentimentDataFrame, 'Polarity Analysis of Tweets on Twitter about Kdrama in Korea')
#wordCloud on emotion
removeCustomeWords <- function (TweetsCleaned) {
  for(i in 1: length(TweetsCleaned)){
    TweetsCleaned[i] <- tryCatch({
      TweetsCleaned[i] = removeWords(TweetsCleaned[i], c(stopwords("english"),
                                                         "care", "guys", "can", "dis", "didn", "guy", "plz"))
      TweetsCleaned[i]},
      error=function(cond) {
        TweetsCleaned[i]},
      warning=function(cond){
        TweetsCleaned[i]
      })
  }
  return(TweetsCleaned)
}

getWordCloud <- function
(sentiment_dataframe, TweetsCleaned, emotion){
  emos = levels(factor(sentiment_dataframe$Emotion))
  n_emos = length(emos)
  emo.docs = rep(" ", n_emos)
  TweetsCleaned = removeCustomeWords(TweetsCleaned)
  
  for (i in 1:n_emos){
    emo.docs[i] = paste(TweetsCleaned[emotion == emos[i]], collapse=" ")
  }
  corpus = Corpus(VectorSource(emo.docs))
  tdm = TermDocumentMatrix(corpus)
  tdm = as.matrix(tdm)
  colnames(tdm) = emos
  require(wordcloud)
  #suppressWarnings(comparison.cloud(tdm, colors=brewer.pal(n=emos,"Dark2"), scale=c(3,5), random.order = FALSE, title.size=1.5))
  suppressWarnings(comparison.cloud(tdm, random.order=FALSE,
                                    colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                                    title.size=1.5, max.words=250))
}

getWordCloud(KpopSentimentDataFrame, KpopTweetsCleaned, KpopEmotion)
getWordCloud(KpopKRSentimentDataFrame, KpopKRTweetsCleaned, KpopKREmotion)
getWordCloud(KdramaSentimentDataFrame, KdramaTweetsCleaned, KdramaEmotion)
getWordCloud(KdramaKRSentimentDataFrame, KdramaKRTweetsCleaned, KdramaKREmotion)



