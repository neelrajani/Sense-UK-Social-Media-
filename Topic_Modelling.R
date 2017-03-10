#Twitter Topic Analysis

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
tweets = searchTwitter("@andigital", n = 3000) #Can also use arguements since and until to get tweets within specific date
# we got a vector of sentences. plyr will handle a list or a vector as an “l” for us
# we want a simple array of scores back, so we use “l” + “a” + “ply” = laply
tweets.text = laply(tweets, function(t)t$getText())
a = iconv(tweets.text, "ASCII", "UTF-8", sub="") #Get rid of emoticons

#Cleaning data (don't want too many words in algorithm and need to use most relevant words)
#install.packages('tm')
#install.packages('SnowballC')
library(tm)
library(SnowballC)
corpus = VCorpus(VectorSource(a))
corpus = tm_map(corpus,content_transformer(tolower)) #need to make them lower case so not as to treat upper case words and lower case words as different words
corpus = tm_map(corpus,removeNumbers) #not relevant to classification and will mess up algorithm
corpus = tm_map(corpus,removePunctuation) #so as to not consider same words as different if they have punctuation in there
corpus = tm_map(corpus,removeWords,stopwords()) #reduces the spare matrix considerably with just useful words remaining
corpus = tm_map(corpus,stemDocument) #reduces the spare matrix considerably as it just considers root words
corpus = tm_map(corpus,stripWhitespace) #when words and punctuation is removed, sometimes they leave spaces so it is worth doing this fucntion so only one space between words
#as.character(corpus[[1]]) can be used to check individual lines of text

#Creating Bag of Words model (Sparsity Matrix)
dtm = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm,0.999) #Keeps the most frequent 99.9% of words (keep this high!)

#Install Topic Modelling library
install.packages('topicmodels')
library(topicmodels)

#Set parameters for Gibbs sampling
burnin = 4000
iter = 2000
thin = 500
seed = list(2003,5,63,100001,765)
nstart = 5
k = 5
best = TRUE

#Run LDA using Gibbs sampling
ldaOut <-LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
ldaOut.topics <- as.matrix(topics(ldaOut))
ldaOut.terms <- as.matrix(terms(ldaOut,6)) #top 6 terms in each topic

#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)