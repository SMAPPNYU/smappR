#' @rdname tweetsToMongo
#' @export
#'
#' @title 
#' Parses and exports tweets to a Mongo DB collection, adding fields useful
#' for their analysis, following the same convention as the python scripts
#' the lab uses to collect tweets.
#'
#' @description
#' \code{tweetsToMongo} read tweets in JSON format (downloaded e.g. using
#' the \code{filterStream} function in the \code{streamR} package), creates
#' an index and other fields that can facilitate their analysis, and stores
#' them in a MongoDB collection
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#'
#' @param file.name string, name of the file where tweets were written. 
#'
#' @param ns string, namespace of the collection to which tweets will be added. Generally,
#' it will be of the form "database.collection". If the database or the collection do not exist,
#' they will be automatically created; if they exist, tweets will be appended.
#'
#' @param host string host/port where mongo database is hosted. Default is localhost (127.0.0.1).
#'
#' @param username string, username to be used for authentication purposes with MongoDB.
#' 
#' @param password string, password corresponding to the given username.
#'
#' @examples \dontrun{
#'  
#' ## An example of an authenticated request using the ROAuth package, 
#' ## where consumerkey and consumer secret are fictitious. 
#' ## You can obtain your own at dev.twitter.com
#'   library(ROAuth)
#'   requestURL <- "https://api.twitter.com/oauth/request_token"
#'   accessURL <- "http://api.twitter.com/oauth/access_token"
#'   authURL <- "http://api.twitter.com/oauth/authorize"
#'   consumerKey <- "xxxxxyyyyyzzzzzz"
#'   consumerSecret <- "xxxxxxyyyyyzzzzzzz111111222222"
#'   my_oauth <- OAuthFactory$new(consumerKey=consumerKey,
#'     consumerSecret=consumerSecret, requestURL=requestURL,
#'     accessURL=accessURL, authURL=authURL)
#'   my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
#'
#' ## capture 10 tweets mentioning the "Rstats" hashtag
#'   library(streamR)
#'   filterStream( file.name="tweets_rstats.json", 
#'      track="rstats", tweets=10, oauth=my_oauth )
#'
#' ## exporting those 10 tweets to MongoDB (database name is "tweets" and
#' ## collection name is "rstates")
#'   tweetsToMongo( file.name="tweets_rstats.json", ns="tweets.rstats",
#'       username="<USERNAME>", password="<PASSWORD>")
#'
#' }
#'

tweetsToMongo <- function(file.name=NULL, ns=NULL, host='localhost', username="", 
	password="", verbose=TRUE)
{

	## from json to list
    results.list <- readTweets(file.name, verbose=FALSE)

    ## connecting to MongoDB
 	db <- strsplit(ns, "\\.")[[1]][1]
 	coll <- strsplit(ns, "\\.")[[1]][2]
	if (verbose==TRUE) { 
		message("Storing tweets in collection '", coll, "' of database '", 
			db, "' in MongoDB") }
	mongo <- mongo.create(host=host, username=username, password=password, db=db)
	if (mongo.get.err(mongo)!=0){ stop("Error in connection to MongoDB") }

    ## dumping into MongoDB
    if (verbose){ message("Preparing tweets for MongoDB")}
    tweets <- lapply(results.list, prepareForMongo)
    todelete <- which(unlist(lapply(tweets, class))=="NULL")
    if (length(todelete)>0){ tweets <- tweets[-todelete]}
    if (verbose){ message("Uploading tweets to MongoDB")}
    if (length(tweets)<=1000){
        mongo.insert.batch(mongo=mongo, ns=ns, tweets)    
    }
    if (length(tweets)>1000){
        indices <- seq(1, length(tweets), 1000)
        sapply(indices,
            function(x)
            mongo.insert.batch(mongo=mongo, ns=ns, 
                tweets[x:ifelse((x+999)>length(tweets), length(tweets), x+999)]))
    }
    
    if (verbose){ message(length(tweets), " tweets saved in MongoDB")}

}


readTweets <- function(tweets, verbose=TRUE){
    ## checking input is correct
    if (is.null(tweets)){
        stop("Error: you need to specify file or object where tweets text was stored.")
    }

    ## Read the text file and save it in memory as a list           
    if (length(tweets)==1 && file.exists(tweets)){
        lines <- readLines(tweets, encoding="UTF-8")
    }       
    else {
        lines <- tweets
    }

    results.list <- lapply(lines[nchar(lines)>0], function(x) 
        tryCatch(rjson::fromJSON(x), error=function(e) e))

    ## removing lines that do not contain tweets or were not properly parsed
    errors <- which(unlist(lapply(results.list, length))<18)
    if (length(errors)>0){
        results.list <- results.list[-errors]
    }

    # information message
    if (verbose==TRUE) cat(length(results.list), "tweets have been parsed.", "\n")
    return(results.list)
}

prepareForMongo <- function(tweet){
    fields <- names(tweet)
    if ('text' %in% fields){
        tweet[['_id']] <- tweet[['id_str']]
        tweet[['timestamp']] <- formatTwDate(tweet[['created_at']])
        tweet[['random_number']] <- runif(1, 0, 1)
    }
    tweet <- mongo.bson.from.list(tweet)
    return(tweet)
}



