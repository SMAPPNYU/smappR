#' @rdname count.tweets
#' @export
#'
#' @title 
#' Connect to Mongo database and return count of tweets that match
#' conditions specified in the arguments.
#'
#' @description
#' \code{count.tweets} opens a connection to the Mongo database in
#' the lab computer and will return the number of tweets that match a series of
#' conditions: whether it contains a certain keyword, whether it is or not a retweet,
#' or whether or not it contains a hashtag.
#'
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#'
#' @param set string, name of the collection of tweets in the Mongo database to query.
#'
#' @param string string or vector of strings, set to NULL by default (will 
#' return count of all tweets). If it is a string, it will return the number
#' of tweets that contain that string. If it is a vector of string, it will
#' return all tweets that contain at least one of them.
#'
#' @param retweets logical, set to NULL by default (will return count of all tweets).
#' If \code{TRUE}, will count only tweets that are retweets (i.e. contain an embededed
#' retweeted status - manual retweets are not included). If \code{FALSE}, will count 
#' only tweets that are not retweets (manual retweets are now included).
#'
#' @param hashtags logical, set to NULL by default (will return count of all tweets).
#' If \code{TRUE}, will count only tweets that use a hashtag. If \code{FALSE}, will
#' count only tweets that do not contain a hashtag. 
#'
#' @param from date, in string format. If different from \code{NULL}, will 
#' consider only tweets after that date. Note that using this field requires that
#' the tweets have a field in ISODate format called \code{timestamp}. All times are GMT.
#'
#' @param to date, in string format. If different from \code{NULL}, will 
#' consider only tweets after that date. Note that using this field requires that
#' the tweets have a field in ISODate format called \code{timestamp}. All times are GMT.
#'
#' @param user_id numeric ID of a user. If different form \code{NULL}, will count
#' only tweets sent by that Twitter user (if there are any in the collection)
#'
#' @param screen_name screen name of a user. If different form \code{NULL}, will count
#' only tweets sent by that Twitter user (if there are any in the collection)
#'
#' @param verbose logical, default is \code{TRUE}, which generates some output to the
#' R console with information about the count of tweets. If code{FALSE}, function will
#' not return any object.
#'
#' @examples \dontrun{
#' ## connect to the Mongo database
#'  mongo <- mongo.create("SMAPP_HOST:PORT", db="DATABASE")
#'  mongo.authenticate(mongo, username="USERNAME", password="PASSWORD", db="DATABASE")
#'  set <- "DATABASE.COLLECTION"
#'
#' ## count all tweets in the database
#'  count.tweets(set)
#'
#' ## count tweets that mention the word 'turkey'
#'  count.tweets(set, string="turkey")
#'
#' ## count tweets that mention the words 'turkey' and 'gezi'
#'  count.tweets(set, string=c("turkey", "gezi"))
#' 
#' ## count all retweets in the database
#'  count.tweets(set, retweets=TRUE)
#'
#' ## count all tweets that mention 'turkey' and are not retweets
#'  count.tweets(set, string="turkey", retweets=FALSE)
#'
#' ## count all tweets that use a hashtag
#'  count.tweets(set, hashtags=TRUE)
#'
#' ## count all tweets from January 1st to January 15th
#'  count.tweets(set, from="2014-01-01 00:00:00", to="2014-01-15 23:59:59")
#' }
#'

## count number of tweets
count.tweets <- function(set, string=NULL, retweets=NULL, hashtags=NULL, 
    from=NULL, to=NULL, user_id=NULL, screen_name=NULL, verbose=TRUE)
{

    require(rmongodb)
    if (exists("mongo", envir=.GlobalEnv) == FALSE){
        stop("R couldn't find mongo object that connects to MongoDB in workspace.")
    }
    query <- list()
    ## querying by string using regex
    if (!is.null(string)){
        if (length(string)>1) { string <- paste(string, collapse='|') }
        query <- c(query, list(text=list('$regex'=string, '$options'='i')))
    }
     ## querying by user
    if (!is.null(user_id)){
        if (length(user_id)==1) { query <- c(query, 
            list(user.id_str=as.character(user_id)))}
        if (length(user_id)>1){
            stop("Error! You can only query tweets sent from one user")
        }
    }
    if (!is.null(screen_name)){
        if (length(screen_name)==1) { query <- c(query, 
            list(user.screen_name=list('$regex'=paste0('^', screen_name), '$options'='i')))}
        if (length(screen_name)>1){
            stop("Error! You can only query tweets sent from one user")
        }
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
    ## querying by date
    if (!is.null(from)){
        from.txt <- as.POSIXct(from, "%Y-%m-%d %H:%M:%S")
        query <- c(query, list(timestamp=list('$gte'=from.txt)))
    }
    if (!is.null(to)){
        to.txt <- as.POSIXct(to, "%Y-%m-%d %H:%M:%S")
        query <- c(query, list(timestamp=list('$lt'=to.txt)))
    }

    ## all tweets if no condition is specified
    if (length(query)==0) query <- mongo.bson.empty()
    ## performing the query
    n.tweets <- mongo.count(mongo, ns=set, query=query)
    if (verbose==TRUE) {cat(n.tweets, "tweets", "\n")}
    if (verbose==FALSE) {return(n.tweets)}

}







