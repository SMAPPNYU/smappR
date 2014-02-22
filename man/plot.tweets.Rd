\name{plot.tweets}
\alias{plot.tweets}
\title{Plots number of tweets by hour or minute}
\usage{
  \method{plot}{tweets} (tweets, breaks = "hours")
}
\arguments{
  \item{tweets}{object of class tweets}

  \item{breaks}{either "minutes" or "hours"}
}
\description{
  Plots number of tweets by hour or minute
}
\examples{
\dontrun{
## connect to the Mongo database
 mongo <- mongo.create("SMAPP_HOST:PORT", db="DATABASE")
 mongo.authenticate(mongo, username="USERNAME", password="PASSWORD", db="DATABASE")
 set <- "DATABASE.COLLECTION"

## extract all tweets that mention 'occupygezi' and do a quick plot
 tweets <- extract.tweets(set, string="occupygezi", fields="created_at")
 plot(tweets)
}
}
\author{
  Pablo Barbera \email{pablo.barbera@nyu.edu}
}

