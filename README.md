`smappR` Package: Tools for analysis of Twitter data
=======

 This package provides a series of functions that allow lab members to access the Mongo database of tweets in the lab computer and easily compute summary statistics. It also includes a function that implements the ideology estimation method developed in Barberá (2013), as well as methods to capture list of friends and followers using multiple OAuth tokens.

 Click [here](slides_tools.pdf) for a summary of tools developed at the SMaPP lab.

# Code examples

This document is intended as "coobook" that provides solutions to common tasks and problems in analyzing social media data collected by the lab.

The first three steps are:

1 - [Install the R packages necessary to do the analysis](#a-installing-r-packages)

2 - [Create your own Twitter OAuth token](#b-creating-your-own-twitter-oauth-token) (only necessary if you plan to collect your own data)

3 - [Connect to the lab Mongo DB server](https://github.com/SMAPPNYU/smappR#c-connecting-to-the-smapp-lab-server)

The rest of the document offers "recipes" for the following situations:

1 - [How can I count the number of tweets that match a set of conditions in a collection?](https://github.com/SMAPPNYU/smappR#1-how-can-i-count-the-number-of-tweets-that-match-a-set-of-conditions-in-a-collection)

2 - [How can I extract a data set of tweets that mention a keyword?](https://github.com/SMAPPNYU/smappR#2-how-can-i-extract-a-data-set-of-tweets-that-mention-a-keyword)

3 - [How can I find the most retweeted tweets in a collection?](https://github.com/SMAPPNYU/smappR#3-how-can-i-find-the-most-retweeted-tweets-in-a-collection)

4 - [How can I visualize tweet volume in a collection?](https://github.com/SMAPPNYU/smappR#4-how-can-i-visualize-tweet-volume-in-a-collection)

5 - [How can I count words in tweets and prepare a word cloud?](https://github.com/SMAPPNYU/smappR#5-how-can-i-prepare-a-word-cloud)

6 - [How can I download all the tweets, followers or friends of any Twitter user?](https://github.com/SMAPPNYU/smappR#6-how-can-i-download-all-the-tweets-followers-or-friends-of-any-twitter-user)

7 - [How can I download profile information about multiple users and parse their location into geographic coordinates?](https://github.com/SMAPPNYU/smappR#7-how-can-i-download-profile-information-about-multiple-users-and-parse-their-location-into-geographic-coordinates)

8 - [How can I estimate the ideology of a Twitter user?](https://github.com/SMAPPNYU/smappR#8-how-can-i-estimate-the-ideology-of-a-twitter-user)

9 - [How can I visualize a network of retweets?](https://github.com/SMAPPNYU/smappR#9-how-can-i-visualize-a-network-of-retweets)

10 - [How can I start my own collection of tweets?](https://github.com/SMAPPNYU/smappR#10-how-can-i-start-my-own-collection-of-tweets)

11 - [How can I save tweets I collected on my own in the lab machine?](https://github.com/SMAPPNYU/smappR#11-how-can-i-save-tweets-i-collected-on-my-own-in-the-lab-machine)

12 - [How can I collect public Facebook data?](https://github.com/SMAPPNYU/smappR#12-how-can-i-collect-public-facebook-data)

## A. Installing R packages

The following block of code will install all packages that you need in order to run the code examples below. Most of them are available on CRAN. Rfacebook and streamR, the packages to capture Facebook and Twitter data, are also available on CRAN, but we will install the most recent version (on GitHub). Finally, we will also install our lab's R package.

    doInstall <- TRUE  # Change to FALSE if you don't want packages installed.
    toInstall <- c("ROAuth", "igraph", "ggplot2", "wordcloud", "devtools", "tm",
        "R2WinBUGS", "rmongodb", "scales")
    if(doInstall){
        install.packages(toInstall, repos = "http://cran.r-project.org")
        library(devtools)
        # R packages to get twitter and Facebook data
        install_github("streamR", "pablobarbera", subdir="streamR")
        install_github("Rfacebook", "pablobarbera", subdir="Rfacebook")
        # smapp R package
        install_github("smappR", "SMAPPNYU")
    }


## B. Creating your own Twitter OAuth token

We will use this token to connect to the Twitter API and download data. Follow these steps:

 - Go to apps.twitter.com/ and sign in
 - Click on "Create New App"
 - Fill name, description, and website (it can be anything, even google.com)
 - Leave "Callback URL" empty
 - Agree to user conditions and enter captcha.
 - Copy consumer key and consumer secret and paste below.

```
library(ROAuth)
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "XXXXXXXXXXXX"
consumerSecret <- "YYYYYYYYYYYYYYYYYYY"
my_oauth <- OAuthFactory$new(consumerKey=consumerKey, consumerSecret=consumerSecret, 
    requestURL=requestURL, accessURL=accessURL, authURL=authURL)
```

 - Run this line and go to the URL that appears on screen

```
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
```

 - Copy and paste the PIN number (6 digits) on the R console
 - Change current folder into a folder where you will save all your tokens

```
setwd("~/Dropbox/credentials/")
```

 - Now you can save oauth token for use in future sessions with R
```
save(my_oauth, file="my_oauth")
```

## C. Connecting to the SMaPP lab server

Before running any of the following sections, we need to make sure to load the smappR library and connect to the Mongo DB in the lab server. (Replace all the works in capital letters with those provided by the lab.)
```
library(smappR)
mongo <- mongo.create("MONGO_HOST:PORT", db="DATABASE")
mongo.authenticate(mongo, username='USERNAME', password='PASSWORD', db="DATABASE")
set <- "DATABASE.COLLECTION"
```

## 1. How can I count the number of tweets that match a set of conditions in a collection?

```
# how many tweets have we captured so far?
count.tweets(set)

# how many tweets mention the shutdown?
count.tweets(set, string="shutdown")

# how many tweets mention the shutdown OR obamacare?
count.tweets(set, string=c("shutdown", "obamacare"))

# how many tweets (not including retweets) mention obamacare?
count.tweets(set, string="obamacare", retweets=FALSE)

# how many tweets were published between December 10 and December 20?
count.tweets(set, from="2013-12-10 00:00:00", to="2013-12-20 23:59:59")

# how many tweets use a hashtag?
count.tweets(set, hashtags=TRUE)

# how many tweets were sent by barack obama?
count.tweets(set, screen_name="barackobama")

# for more info:
?count.tweets
```

## 2. How can I extract a data set of tweets that mention a keyword?

Assuming that we're still connected to the mongo DB (see section 1)

```
# how can I extract all tweets that mention "drone" in 2013?
tweets <- extract.tweets(set, string="drone", from="2013-01-01 00:00:00", to="2013-12-31 23:59:59")

# how can I see the first few tweets that were extracted?
df <- tweetsToDF(tweets) ## transforming to data frame
head(df)

# how can I see who has tweeted more about drones?
head(sort(table(df$user.screen_name), dec=TRUE))

# how can I save them to .csv?
write.csv(df, "drone_tweets.csv", row.names=F)

# how can I save to Stata?
library(foreign)
write.dta(df, "drone_tweets.dta")

# what if I also want the retweet count and the coordinates of each tweet?
tweets <- extract.tweets(set, string="drone",  fields = c("created_at", "user.screen_name", 
    "text", "retweeted_status.retweet_count", "geo.coordinates"))

# what fields can I include here?
?extract.tweets

# how can I extract a random sample of 1,000 tweets from the collection?
tweets <- extract.tweets(set, size=1000)

# how can I extract a random sample of 1,000 tweets sent on February 20?
tweets <- extract.tweets(set, size=1000, from="2014-02-20 00:00:00", to="2014-02-20 23:59:59")

# how can I extract a random sample of 100 tweets that mention obamacare?
tweets <- extract.tweets(set, size=100, string="obamacare")

# how can I extract a random sample of 100 tweets that mention obamacare and
# are NOT retweets?
tweets <- extract.tweets(set, size=100, string="obamacare", retweets=FALSE)

# how can I extract a random sample of 0.5% of the entire collection?
tweets <- extract.tweets(set, size=0.005)

# how can I extract tweets that were sent by barack obama?
tweets <- extract.tweets(set, screen_name="barackobama")

# more info:
?extract.tweets
```


## 3. How can I find the most retweeted tweets in a collection?

Assuming that we're still connected to the mongo DB (see section 1)

```
## how can I extract all tweets with more than 5000 retweets?
rts <- extract.retweets(set, min=5000)

## how can I find the 10 most retweeted?
summary(rts, n=10)

## how can I find the most retweeted tweet that mentions drones?
rts <- extract.retweets(set, min=10, string="drone")
summary(rts, n=1)

## how can I find the most retweeted tweet on February 20?
rts <- extract.retweets(set, min=5000, from="2014-02-20 00:00:00", to="2014-02-20 23:59:59")
summary(rts, n=1)

# BONUS:
## how can I find the most used hashtags in a collection?
ht <- extract.hashtags(set)
summary(ht, n=10)

## what hashtags are used in tweets that mention obamacare?
ht <- extract.hashtags(set, string="obamacare")
summary(ht, n=10)

## what were the 10 most common hashtags used on February 20?
ht <- extract.hashtags(set, from="2014-02-20 00:00:00", to="2014-02-20 23:59:59")
summary(ht, n=10)

## for more info:
?extract.retweets
?extract.hashtags
```


## 4. How can I visualize tweet volume in a collection?

Assuming that we're still connected to the mongo DB (see section 1)

```
# how can I download the 10,000 most recent tweets in a collection?
tweets <- extract.recent.tweets(set, fields=c('lang', 'created_at'), limit=1000000)

# and visualize volume by hour
plot(tweets, breaks="hours")

# or per day
plot(tweets, breaks="days")

# how can I extract all tweets that mention "drone"?
tweets <- extract.tweets(set, string="drone")

# how can I look at their distribution over time?
plot(tweets, breaks="days")

# how can I look at the distribution of tweets over a certain day?
tweets <- extract.tweets(set, from="2014-02-20 00:00:00", to="2014-02-20 23:59:59")
plot(tweets, breaks="hours")

```

## 5. How can I prepare a word cloud?

```
# how can I prepare a word cloud of recent tweets?
tweets <- extract.recent.tweets(set, limit=5000)
wordFreq <- word.frequencies(tweets$text) ## word counts

library(wordcloud)
wordcloud(words=names(wordFreq), freq=wordFreq, max.words=50, 
    random.order=F, colors="black", scale=c(2.5,.5), rot.per=0)

# how can I prepare a word cloud of tweets from a given day?
tweets <- extract.tweets(set, from="2014-02-20 00:00:00", to="2014-02-20 23:59:59")
df <- tweetsToDF(tweets)

wordFreq <- word.frequencies(df$text) ## word counts
wordcloud(words=names(wordFreq), freq=wordFreq, max.words=50, 
    random.order=F, colors="black", scale=c(2.5,.5), rot.per=0)

# another example, for tweets mentioning drones
tweets <- extract.tweets(set, string="drone")
wordFreq <- word.frequencies(tweets$text, stopwords=c("amp", "cant", "drones"))
wordcloud(words=names(wordFreq), freq=wordFreq, max.words=50, 
    random.order=F, colors="black", scale=c(2.5,.5), rot.per=0)

# word cloud for recent tweets of any user
getTimeline(screen_name = "p_barbera",
    filename = "pablo_tweets.json", # where tweets will be stored
    n=500, ## number of tweets to download (max is 3,200)
    oauth_folder = "~/Dropbox/credentials" )

library(streamR)
tweets <- parseTweets("pablo_tweets.json") ## reading tweets in R

wordFreq <- word.frequencies(tweets$text)

wordcloud(words=names(wordFreq), freq=wordFreq, max.words=50, 
    random.order=F, colors="black", scale=c(2.5,.5), rot.per=0)

wordFreq <- word.frequencies(tweets$text, 
        stopwords=c("que", "@p_barbera"))
wordcloud(words=names(wordFreq), freq=wordFreq, max.words=50, 
    random.order=F, colors="black", scale=c(2.5,.5), rot.per=0)

# for more info, again:
?getTimeline
?formatTwDate
?extract.recent.tweets
```

## 6. How can I download all the tweets, followers or friends of any Twitter user?

```
# downloading recent tweets sent by a user
getTimeline(screen_name = "p_barbera",
    filename = "pablo_tweets.json", # where tweets will be stored
    n=500, ## number of tweets to download (max is 3,200)
    oauth_folder = "~/Dropbox/credentials" )

# downloading list of followers of a given user
getFollowers(screen_name = "p_barbera", oauth_folder="~/Dropbox/credentials")

# downloading list of friends of a given user
getFriends(screen_name = "p_barbera", oauth_folder="~/Dropbox/credentials")

# BONUS: count the most common hashtags sent by a user
tweets <- parseTweets("pablo_tweets.json")
ht <- extract.hashtags(text=tweets$text)
summary(ht, n=10)
```

## 7. How can I download profile information about multiple users and parse their location into geographic coordinates?

```
# downloading information about multiple users
users.data <- getUsers(screen_names=c("p_barbera", "j_a_tucker", "smapp_nyu"),
    oauth_folder = "~/Dropbox/credentials")

# parsing locations into coordinates
locations <- list()
for (u in 1:length(users.data)){
    locations[[u]] <- getGeo(users.data[[u]][['location']])
}

```

## 8. How can I estimate the ideology of a Twitter user?

```
# step 1: downloading list of friends for a user
user <- "p_barbera"
friends <- getFriends(screen_name=user, oauth_folder="~/Dropbox/credentials/")

# step 2: estimate ideology
results <- estimate.ideology(user, friends)

# display trace plot to check convergence
traceplot.ideology(results)

# comparing with other ideology estimates
ideology.plot(results)

# downloading list of followers of a given user
followers <- getFollowers(screen_name=user, oauth_folder="~/Dropbox/credentials/")
```

## 9. How can I visualize a network of retweets?

```
# 1) download last 10,000 retweets, with author of retweet and author of
# original tweet
tweets <- extract.recent.tweets(set, limit=1000, 
    fields=c("user.id_str", "retweeted_status.user.id_str"),
    retweets=TRUE)

# 2) convert to data frame
df <- tweetsToDF(tweets)
names(df) <- c("Source", "Target")

# 3) preparing data for network visualization
library(igraph)
g <- graph.edgelist(as.matrix(df), directed=TRUE) ## network object
set.seed(12345)
l <-  layout.fruchterman.reingold(g, niter=2000, coolexp=20) ## layout
d <- data.frame(l); names(d) <- c("x", "y") ## data frame for plot
d$id <- V(g)$name ## adding names
d$degree <- degree(g)
d <- merge(d, data, all.x=TRUE) ## adding party

edgelist <- get.edgelist(g, names=FALSE) ## edgelist
edges <- data.frame(d[edgelist[,1],c("x", "y")], d[edgelist[,2],c("x", "y")])
names(edges) <- c("x1", "y1", "x2", "y2") ## coordinates of each edge
edges$party <- NA

# 4) creating plot with ggplot2

library(ggplot2)
p <- ggplot(d, aes(x=x, y=y, fill=party, size=degree))
pq <- p + geom_segment(
        aes(x=x1, y=y1, xend=x2, yend=y2), 
        data=edges, color="grey20", size=0.25, alpha=1/10) +
        geom_point(aes(size=degree), color="grey20", shape=21) +
        scale_size_continuous(range=c(2,7)) +
        scale_fill_manual(values=c("blue", "green", "red")) + 
        theme(
            panel.background = element_blank(),
            plot.background = element_blank(),
            axis.line = element_blank(), axis.text = element_blank(), 
            axis.ticks = element_blank(), 
            axis.title = element_blank(), panel.border = element_blank(), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank()
            )
pq


# 6) exporting data to file to work with Gephi
write.csv(df, file="data/edgelist_congress.csv", row.names=F)
```


## 10. How can I start my own collection of tweets?

```
# loading library and OAuth token
library(streamR)
load("~/Dropbox/credentials/my_oauth")

# How can I collect all tweets that mention certain keywords?
filterStream(file.name="obama_tweets.json", track=c("obama", "romney"), oauth=my_oauth,
    tweets=50)

# How can I then see the tweets I collected?
tweets <- parseTweets("obama_tweets.json")

# a few quick analyses of the data
table(tweets$lang) ## distribution by language
sum(!is.na(tweets$lat)) ## how many are geolocated
summary(tweets$retweet_count) ## how many RTs they have
tweets$text[which.max(tweets$retweet_count)] ## most RTed tweet

# How can I collect a random sample of tweets?
sampleStream(file.name="random_tweets.json", oauth=my_oauth,
    tweets=100)

# same quick analysis
tweets <- parseTweets("random_tweets.json")
table(tweets$lang) ## distribution by language
sum(!is.na(tweets$lat)) ## how many are geolocated
summary(tweets$retweet_count) ## how many RTs they have
tweets$text[which.max(tweets$retweet_count)] ## most RTed tweet

# How can I collect geolocated tweets
filterStream(file.name="geo_tweets.json", oauth=my_oauth,
    locations=c(-180,-90,180,90), tweets=100)
```

## 11. How can I save tweets I collected on my own in the lab machine?

```
# Assume we want to save the geolocated tweets we just captured
tweetsToMongo(file.name="geo_tweets.json", ns="tweets.geotweets",
    host="HOST", username="USERNAME", password="PASSWORD")

# this will dump the tweets onto the lab machine, adding the extra fields
# necessary to get random samples, quick sampling by date, etc.
```


## 12. How can I collect public Facebook data?

```
# loading library
library(Rfacebook)

# adding your OAuth token
# 1) go to https://developers.facebook.com/tools/explorer/
# 2) click on "Get Access Token"
# 3) copy and paste below:
token <- "XXXXXX"

###############################################################################
## PART 1: COLLECTING PUBLIC FACEBOOK POSTS
###############################################################################

# How can I collect public Facebook posts that mention a given keyword?
posts <- searchFacebook("obama", token, n=100)

# How can I see the content of these posts?
posts[1,]

# Which of these posts got more likes?
posts[which.max(posts$likes_count),]

# How can I get more information about the users who posted about Obama?
users <- getUsers(posts$from_id, token)

# What information can I get about these users?
users[1,]

## NOTE: this information is available for ALL users, not only those who
## post stuff publicly
getUsers("pablobarbera", token)

# From what country are users posting about Obama?
countries <- substr(users$locale, 4, 5)
table(countries)

# What is the gender of users posting about Obama?
table(users$gender, exclude=NULL) ## The NA correspond to 'pages' (not users)

# In what language are users posting about Obama?
languages <- substr(users$locale, 1, 2)
table(languages)

# How can I collect old public Facebook posts?
posts <- searchFacebook("shutdown", token, n=1000, since="17 october 2013 02:00:00",
    until="17 october 2013 03:00:00")  ## usually pretty slow
# (note that all times are in GMT, so this would be 9pm to 10pm DC time)

# Which of these posts got more likes?
posts[which.max(posts$likes_count),]

# How can I generate a word cloud of the content of these posts?
library(smappR)
library(wordcloud)
wordFreq <- word.frequencies(posts$message)
wordcloud(words=names(wordFreq), freq=wordFreq, max.words=50, 
    random.order=F, colors="black", scale=c(2.5,.5), rot.per=0)


# more details:
?searchFacebook
?getUsers

###############################################################################
## PART 2: SCRAPING INFORMATION FROM FACEBOOK PAGES
###############################################################################

# How can I get a list of posts from a Facebook page?
page <- getPage("barackobama", token, n=100) ## ~100 most recent posts

# What information is available for each of these posts?
page[1,]

# Which post got more likes?
page[which.max(page$likes_count),]

# Which post got more comments?
page[which.max(page$comments_count),]

# Which post was shared the most?
page[which.max(page$shares_count),]

# How can I get a list of users who liked a specific post?
post <- getPost(page$id[1], token, n=2000) # first post, get 2000 likes/comments

# How can I see the gender, country, and language of these users?
users <- getUsers(post$likes$from_id, token)
countries <- substr(users$locale, 4, 5)
table(countries)
table(users$gender, exclude=NULL)
languages <- substr(users$locale, 1, 2)
table(languages)

# What are the most common first names?
head(sort(table(users$first_name), dec=TRUE), n=10)

# Compare these results to data from Sarah Palin's Facebook page...
page <- getPage("sarahpalin", token, n=100)
page[which.max(page$comments_count),] # most "commented" post
post <- getPost(page$id[1], token, n=2000, comments=FALSE, likes=TRUE)
users <- getUsers(post$likes$from_id, token) # user info for 2,000 users who
                                             # liked most recent post
countries <- substr(users$locale, 4, 5)
table(countries)
table(users$gender, exclude=NULL)
languages <- substr(users$locale, 1, 2)
table(languages)

```
