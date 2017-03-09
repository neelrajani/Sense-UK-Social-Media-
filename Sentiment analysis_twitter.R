#Twitter sentiment analysis for Sense UK

#Installing TwitteR package
install.packages("twitteR")
library(twitteR)
consumer_key <- "Lt3tNBPAiH3Ux6UZgSERFprun"
consumer_secret <- 	"TC6uVEC3dhI9Dc90FaoqKSkL2yMupqGFC9SR0wONxTrhajQAvP"
access_token <- 	"1553212526-AeL3nH6RHySSkiaOniopXJ5Z8h8Ab6dBnE28uTt"
access_secret <- 	"jLWMw07vYvoTqAp5JcyqfJczd6bblUhaLaUsb2I5g8teq"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#Extracting tweets
install.packages('plyr')
install.packages('stringr')
library(plyr)
library(stringr)
tweets = searchTwitter("#muslimban", n = 3200) #Can also use arguements since and until to get tweets within specific date
# we got a vector of sentences. plyr will handle a list or a vector as an “l” for us
# we want a simple array of scores back, so we use “l” + “a” + “ply” = laply
tweets.text = laply(tweets, function(t)t$getText())
a = iconv(tweets.text, "ASCII", "UTF-8", sub="")

#Allowing R to read the positive and negative words
pos = scan('positive-words.txt', what='character',comment.char=';')
neg = scan('negative-words.txt', what='character',comment.char=';')

#Sentiment scoring function building
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
# we got a vector of sentences. plyr will handle a list or a vector as an “l” for us
# we want a simple array of scores back, so we use “l” + “a” + “ply” = laply:
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    # clean up sentences with R’s regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]',"", sentence)
    sentence = gsub('[[:cntrl:]]',"", sentence)
    sentence = gsub('\\d+', "", sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches)-sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

#Completing the sentiment analysis using the new function
analysis = score.sentiment(a, pos, neg, .progress='none')

#Examining the analysis score and number of tweets per score
table(analysis$score)

#Relationship between sentiment score and engagements
#Summing tweet engagements
tweets.favorite = laply(tweets, function(t)t$getFavoriteCount())
tweets.retweet = laply(tweets, function(t)t$getRetweetCount())
#Adding them to analysis data table
analysis$engagements = tweets.favorite + tweets.retweet

#Fitting Regression Model to Dataset
#install.packages("e1071")
library(e1071)
regressor = svm(engagements ~ score, 
                data = analysis,
                type = 'eps-regression')

# Visualising the SVR Regression Model results
# install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x = analysis$score, y = analysis$engagements),
             colour = 'red') +
  geom_line(aes(x = analysis$score, y = predict(regressor, newdata = analysis)),
            colour = 'blue') +
  ggtitle('Sentiment score v engagements (SVR Regression Model)') +
  xlab('Score') +
  ylab('Engagements')
