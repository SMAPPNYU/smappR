#' @rdname extract.recent.tweets
#' @export
#'
#' @title 
#' Connect to Mongo database and extract most recent tweets for a given
#' collection
#'
#' @description
#' \code{extract.recent.tweets} opens a connection to the Mongo database in
#' the lab computer and will most recent tweets.
#'
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#'
#' @param set string, name of the collection of tweets in the Mongo database to query.
#'
#' @param fields vector of strings, indicates fields from tweets that will be 
#' returned. Default is the date and time of the tweet, its text, and the screen
#' name of the user that published it. See details for full list of possible fields.
#'
#' @param limit number of tweets to be returned. Default is 100, which will return
#' 100 most recent tweets.
#'
#' @param verbose logical, default is \code{TRUE}, which generates some output to the
#' R console with information about the count of tweets.
#'
#' @param string string or vector of strings, set to NULL by default (will 
#' return all recent tweets). If it is a string, it will return the number
#' of tweets that contain that string. If it is a vector of string, it will
#' return all tweets that contain at least one of them.
#'
#' @param retweets logical, set to NULL by default (will return all tweets).
#' If \code{TRUE}, will return only tweets that are retweets (i.e. contain an embededed
#' retweeted status - manual retweets are not included). If \code{FALSE}, will return 
#' only tweets that are not retweets (manual retweets are now included).
#'
#' @param hashtags logical, set to NULL by default (will return all tweets).
#' If \code{TRUE}, will return only tweets that use a hashtag. If \code{FALSE}, will
#' return only tweets that do not contain a hashtag. 
#'
#'
#' @details
#' The following is a non-exhaustive of relevant fields that can be specified on the 
#' \code{fields} argument (for a complete list, check the documentation at:
#' \url{https://dev.twitter.com/docs/platform-objects}
#' Tweet: text, created_at, id_str, favorite_count, source, retweeted, r
#' retweet_count, lang, in_reply_to_status_id, in_reply_to_screen_name
#' Entities: entities.hashtags, entities.user_mentions, entities.hashtags, entities.urls
#' Retweeted_status: retweeted_status.text, retweeted_status.created_at... (and all 
#' other tweet, user, and entities fields)
#' User: user.screen_name, user.id_str, user.geo_enabled, user.location, 
#' user.followers_count, user.statuses_count, user.friends_count, 
#' user.description, user.lang, user.name, user.url, user.created_at, user.time_zone 
#' Geo: geo.coordinates
#'
#' @examples \dontrun{
#' ## connect to the Mongo database
#'  mongo <- mongo.create("SMAPP_HOST:PORT", db="DATABASE")
#'  mongo.authenticate(mongo, username="USERNAME", password="PASSWORD", db="DATABASE")
#'  set <- "DATABASE.COLLECTION"
#'
#' ## extract text from 100 most recent tweets
#'  tweets <- extract.recent.tweets(set, limit=100, fields="text")
#' 
#' ## extract 100 most recent tweets that mention turkey and gezi
#'  tweeets <- extract.recent.tweets(set, limit=100, 
#'    string=c("turkey", "gezi"))
#' }
#'

## return recent tweets
extract.recent.tweets <- function(set, limit=100, 
    fields=c('created_at', 'user.screen_name', 'text'), 
    string=NULL, retweets=NULL, hashtags=NULL, verbose=TRUE)
{

    fields.arg <- fields
    query <- list()

    ## querying by string using regex
    if (!is.null(string)){
        if (length(string)>1) { string <- paste(string, collapse='|') }
        query <- c(query, list(text=list('$regex'=string, '$options'='i')))
    }

    ## adding retweets and hashtag condition
    if (!is.null(retweets)){
        if (retweets==TRUE){
            query <- c(query, list(retweeted_status=list('$exists'=TRUE)))
        }
        if (retweets==FALSE){
            query <- c(query, list(retweeted_status=list('$exists'=FALSE)))
        }
    }
    if (!is.null(hashtags)){
        if (hashtags==TRUE){
            query <- c(query, list(entities.hashtags.1=list('$exists'=TRUE)))
        }
        if (hashtags==FALSE){
            query <- c(query, list(entities.hashtags.1=list('$exists'=FALSE)))
        }
    }

    ## all tweets if no condition is specified
    if (length(query)==0) query <- mongo.bson.empty()

    # preparing empty vector
    n <- mongo.count(mongo, ns=set, query=query)
    if (n==0 | n==-1){
        stop("Zero tweets match the specified conditions. Is the DB name correct?")
    }
    if (limit > n){ 
        message(limit, " tweets were requested, but only ", n, " were found" )
        limit <- n
    }
    out <- rep(NA, limit)
  

    ## adding fields
    if (is.null(fields)) {fields <- mongo.bson.empty()}
    if (!is.null(fields)) {
        new.fields <- list()
        for (f in fields){
            new.fields[f] <- 1L
        }
        fields <- new.fields
    }

    # making query
    res <- mongo.find(mongo=mongo, ns=set, query=query, fields=fields,
        sort=list('_id'=-1L), limit=as.integer(limit))
    i <- 1
    if (verbose==TRUE) {pb <- txtProgressBar(min=1,max=limit, style=3)}
    
    while (mongo.cursor.next(res)) {
        out[i] <- list(mongo.bson.to.list(mongo.cursor.value(res)))
        i <- i + 1
        if (verbose==TRUE) {setTxtProgressBar(pb, i)}
    }

    out <- parseMongo(out, fields=fields.arg)
    class(out) <- "tweets"
    return(out)

}



