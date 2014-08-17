#' @rdname extract.retweets
#' @export
#'
#' @title 
#' Connect to Mongo database and extract retweets that match 
#' conditions specified in the arguments.
#'
#' @description
#' \code{extract.tweets} opens a connection to the Mongo database in
#' the lab computer and will return all retweets, or only retweets that 
#' mention a specific keyword. In combination with \code{summary.retweets},
#' this is a quick way to display the most retweeted tweets over a certain
#' period of time.
#'
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#'
#' @param set string, name of the collection of tweets in the Mongo database to query.
#'
#' @param string string or vector of strings, set to NULL by default (will 
#' return all retweets). If it is a string, it will return retweets that 
#' contain that string. If it is a vector of string, it will
#' return all tweets that contain at least one of them.
#'
#' @param min numeric, set to 10 by default (will return all retweets whose
#' retweet count is at least 10). In large datasets, choose a high number to
#' increase speed of query.
#'
#' @param from date, in string format. If different from \code{NULL}, will 
#' consider only tweets after that date. Note that using this field requires that
#' the tweets have a field in ISODate format called \code{timestamp}. All times are GMT.
#'
#' @param to date, in string format. If different from \code{NULL}, will 
#' consider only tweets after that date. Note that using this field requires that
#' the tweets have a field in ISODate format called \code{timestamp}. All times are GMT.
#'
#' @param verbose logical, default is \code{TRUE}, which generates some output to the
#' R console with information about the count of tweets.
#'
#' @details
#' Note that this function will only return retweets that are made using the built-in
#' retweeting system - this is, 'manual' retweets using copy&paste are not included.
#' Also note that total retweet counts are based on Twitter's internal tally, and do
#' not reflect the number of retweets in the database. In other words, it could happen
#' that the most popular retweet in a given moment is a tweet that was originally sent
#' days ago, but was retweeted during the time of that tweets were captured.
#'
#' @examples \dontrun{
#' ## connect to the Mongo database
#'  mongo <- mongo.create("SMAPP_HOST:PORT", db="DATABASE")
#'  mongo.authenticate(mongo, username="USERNAME", password="PASSWORD", db="DATABASE")
#'  set <- "DATABASE.COLLECTION"
#'
#' ## extract all retweets that were retweeted at least 2000 times
#'  rts <- extract.retweets(set, min=2000)
#'
#' ## show top 10 retweets from previous query
#'  summary(rts, n=10)
#'
#' ## extract all retweets that mentioned "turkey" and were retweeted at least 100 times
#'  rts <- extract.retweets(set, string="occupygezi", min=100)
#'
#' ## show top 10 retweets from previous query
#'  summary(rts, n=10)
#' }
#'
#'


extract.retweets <- function(set, string=NULL, min=10, from=NULL, to=NULL, verbose=TRUE)
{
    query <- list(retweeted_status=list('$exists'=TRUE), retweeted_status.retweet_count=list('$gte'=min))
    ## querying by date
    if (!is.null(from)){
        from.txt <- as.POSIXct(from, "%Y-%m-%d %H:%M:%S")
        query <- c(query, list(timestamp=list('$gte'=from.txt)))
    }
    if (!is.null(to)){
        to.txt <- as.POSIXct(to, "%Y-%m-%d %H:%M:%S")
        query <- c(query, list(timestamp=list('$lt'=to.txt)))
    }
    ## querying by string using regex
    if (!is.null(string)){
        if (length(string)>1) { string <- paste(string, collapse='|') }
        query <- c(query, list(text=list('$regex'=string, '$options'='i')))
    }
    n <- mongo.count(mongo, ns=set, query=query)
    out <- rep(NA, n)
    # making query
    res <- mongo.find(mongo=mongo, ns=set, query=query, fields=list(retweeted_status=1L))
    i <- 1
    if (verbose==TRUE) {pb <- txtProgressBar(min=1,max=n, style=3)}
    while (mongo.cursor.next(res)) {
        out[i] <- list(mongo.bson.to.list(mongo.cursor.value(res)))
        i <- i + 1
        if (verbose==TRUE) {setTxtProgressBar(pb, i)}
    }
    tweets <- lapply(out, "[[", "retweeted_status")
    tweets.ids <- unlist(lapply(tweets, '[', "id_str"))
    dups <- duplicated(tweets.ids, fromLast=TRUE)
    tweets <- tweets[!dups]
    sorted <- order(unlist(lapply(tweets, '[[', "retweet_count")), decreasing=TRUE)
    tweets <- tweets[sorted]
    if (verbose==TRUE) {
        cat("\n", length(tweets), "unique retweets with more than", min, "hits, after removing duplicates")
    }
    class(tweets) <- "retweets"
    return(tweets)
}

#' @export
## summary of retweets
summary.retweets <- function(retweets, n=10){
    data.frame(cbind(screen_name = lapply(lapply(retweets[1:n], '[[', "user"), '[[', "screen_name"),
          text = lapply(retweets[1:n], '[[', "text"),
          retweet_count = lapply(retweets[1:n], '[[', "retweet_count")), stringsAsFactors=F)
}




