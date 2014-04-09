\name{format.twitter.date}
\alias{format.twitter.date}
\title{Converts from Twitter date format to R date format}
\usage{
\method{format}{twitter.date}(datestring, format = "datetime")
}
\arguments{
  \item{datestring}{Date string in Twitter format}

  \item{output}{Either "date", for Date format; or
  "datetime" for POSIX.}
}
\description{
Converts from Twitter date format to R date format
}
\examples{
\dontrun{
## connect to the Mongo database
 mongo <- mongo.create("SMAPP_HOST:PORT", db="DATABASE")
 mongo.authenticate(mongo, username="USERNAME", password="PASSWORD", db="DATABASE")
 set <- "DATABASE.COLLECTION"

## extract all tweets that mention 'occupygezi' and do a quick plot
 tweets <- extract.tweets(set, string="occupygezi", fields="created_at")

## convert dates to R format
 dates <- format.twitter.date(tweets$created_at, format="date")

## see number of tweets per day
 table(dates)
}
}
\author{
Pablo Barbera \email{pablo.barbera@nyu.edu}
}

