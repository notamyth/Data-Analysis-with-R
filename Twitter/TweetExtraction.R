#Clear R Environment
rm(list=ls())
#Load required libraries
install.packages(c("devtools", "rjson", "bit64", "httr"))
install.packages("twitteR")
install.packages("ROAuth")
install.packages("httpuv")
library(httpuv)
library(devtools)
install_github("geoffjentry/twitteR")
library("twitteR")
library("ROAuth")


setup_twitter_oauth("6ClQkIOqatJNprrwLGFH9KhLU", "y3fMXj66ruwvA6dVeyw9YcnmX7cFJkx345BDpRWDEFQMrGFa88") 

api_key <- ' 6ClQkIOqatJNprrwLGFH9KhLU'
api_secret <- ' y3fMXj66ruwvA6dVeyw9YcnmX7cFJkx345BDpRWDEFQMrGFa88'
requestURL='https://api.twitter.com/oauth/request_token'
accessURL='https://api.twitter.com/oauth/access_token'
authURL='https://api.twitter.com/oauth/authorize'
access_token <- ' 58730036-cnqKB326gTg69e2T8Gkg2aouJfyQMpRnp4TOuJO7J'
access_token_secret <- 'WpyV5AJWEk0XouHDYOH8U3K9ORiAAlSfNVqqu7N98O4LF'

setup_twitter_oauth('6ClQkIOqatJNprrwLGFH9KhLU', 'y3fMXj66ruwvA6dVeyw9YcnmX7cFJkx345BDpRWDEFQMrGFa88', '58730036-cnqKB326gTg69e2T8Gkg2aouJfyQMpRnp4TOuJO7J', 'WpyV5AJWEk0XouHDYOH8U3K9ORiAAlSfNVqqu7N98O4LF')
tweets <- searchTwitter('#arsenal', n=150) 
tweets.df <- do.call(rbind, lapply(tweets, as.data.frame))
write.csv(tweets.df, "Tweets.csv")
tweets

## Unused Code##

# Download the file and store in your working directory
#download.file(url= "http://curl.haxx.se/ca/cacert.pem", destfile= "cacert.pem")


#Insert your consumerKey and consumerSecret below
#credentials <- OAuthFactory$new(consumerKey=' 6ClQkIOqatJNprrwLGFH9KhLU',
#                                consumerSecret=' y3fMXj66ruwvA6dVeyw9YcnmX7cFJkx345BDpRWDEFQMrGFa88',
#                                requestURL='https://api.twitter.com/oauth/request_token',
#                                accessURL='https://api.twitter.com/oauth/access_token',
#                                authURL='https://api.twitter.com/oauth/authorize')
#credentials$handshake(cainfo="cacert.pem")
#credentials$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
#save(credentials, file="twitter authentication.Rdata")
#Load Authentication Data
#load("twitter authentication.Rdata")
#Register Twitter Authentication
#setup_twitter_oauth(credentials$consumerKey, credentials$consumerSecret, credentials$oauthKey, credentials$oauthSecret)
#Extract Tweets with concerned string(first argument), followed by number of tweets (n) and language (lang)
#tweets <- searchTwitter('#Arsenal', n=10, lang="en")
