#' @rdname extract.tweets
#' @export
#'
#' @title 
#' Connect to Mongo database and extract tweets that match 
#' conditions specified in the arguments.
#'
#' @description
#' \code{extract.tweets} opens a connection to the Mongo database in
#' the lab computer and will return tweets that match a series of conditions: 
#' whether it contains a certain keyword, whether it is or not a retweet,
#' or whether or not it contains a hashtag. It allows to specify the fields of
#' the tweet to be extracted. If desired, it can also return a fixed number of
#' tweets that will represent a random sample of all tweets in the database.
#'
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#'
#' @param set string, name of the collection of tweets in the Mongo database to query.
#'
#' @param string string or vector of strings, set to NULL by default (will 
#' return all tweets). If it is a string, it will return tweets that contain 
#' that string. If it is a vector of string, it will
#' return all tweets that contain at least one of them.
#'
#' @param size numeric, set to 0 by default (will return all tweets that match
#' other conditions). If it between 0 and 1 (not included), it will return that
#' proportion of tweets in the database (e.g. 0.5 implies 50\% of all tweets that
#' match other conditions will be returned). If it is 1 or greater, it will return
#' a random sample of that size with tweets that match the specified conditions.
#'
#' @param fields vector of strings, indicates fields from tweets that will be 
#' returned. Default is the date and time of the tweet, its text, and the screen
#' name of the user that published it. See details for full list of possible fields.
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
#' @param from date, in string format. If different from \code{NULL}, will 
#' consider only tweets after that date. Note that using this field requires that
#' the tweets have a field in ISODate format called \code{timestamp}. All times are GMT.
#'
#' @param to date, in string format. If different from \code{NULL}, will 
#' consider only tweets after that date. Note that using this field requires that
#' the tweets have a field in ISODate format called \code{timestamp}. All times are GMT.
#'
#' @param user_id vector of numeric IDs for users. If different form \code{NULL}, will return
#' only tweets sent by that set of Twitter users (if there are any in the collection)
#'
#' @param screen_name screen name of a user. If different form \code{NULL}, will return
#' only tweets sent by that Twitter user (if there are any in the collection)
#'
#' @param verbose logical, default is \code{TRUE}, which generates some output to the
#' R console with information about the count of tweets.
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
#' ## extract text from all tweets in the database
#'  tweets <- extract.tweets(set, fields="text")
#' 
#' ## extract random sample of 10% of tweets, with text and screen name
#'  tweets <- extract.tweets(set, fields=c("user.screen_name", "text"), size=0.10)
#'
#' ## extract random sample of 100 tweets that are not retweets
#'  tweets <- extract.tweets(set, size=100, retweets=FALSE)
#'
#' ## extract all tweets that mention turkey
#'  tweets <- extract.tweets(set, string="turkey")
#'
#' ## extract all tweets that mention 'occupygezi' and do a quick plot
#'  tweets <- extract.tweets(set, string="occupygezi", fields="created_at")
#'  plot(tweets)
#' }
#'

extract.tweets <- function(set, string=NULL, size=0, 
    fields=c('created_at', 'user.screen_name', 'text'),
    retweets=NULL, hashtags=NULL, from=NULL, to=NULL, user_id=NULL, 
    screen_name=NULL, verbose=TRUE)
{

    require(rmongodb)
    fields.arg <- fields
    query <- list()
    
    ## querying by date
    if (!is.null(from)){
        from.txt <- as.POSIXct(from, "%Y-%m-%d %H:%M:%S", tz="")
        query <- c(query, list(timestamp=list('$gte'=from.txt)))
    }
    if (!is.null(to)){
        to.txt <- as.POSIXct(to, "%Y-%m-%d %H:%M:%S", tz="")
        query <- c(query, list(timestamp=list('$lt'=to.txt)))
    }

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
            query <- c(query, list(user.id_str=list('$in'=as.character(user_id))))
        }
    }
    if (!is.null(screen_name)){
        if (length(screen_name)==1) { query <- c(query, 
            list(user.screen_name=list('$regex'=paste0('^', screen_name), '$options'='i')))}
        if (length(screen_name)>1){
            stop("Error! You can only query tweets sent from one user with screen_name. User user_id instead.")
        }
    }
    
    ## adding size of random sample
    if (size>0 & size<1){
        seed <- runif(1, 0, 1-size)
        query <- c(query, list(random_number=list('$gte'=seed, '$lte'=seed+size)))
    }
    if (size>=1){
        if (!is.null(string)){ n.tweets <- count.tweets(set, string=string, verbose=FALSE) }
        if (is.null(string)){ n.tweets <- count.tweets(set, verbose=FALSE) }
        p.diff <- (size / n.tweets)
        seed <- runif(1, 0, 1-p.diff)
        query <- c(query, list(random_number=list('$gte'=seed, '$lte'=seed+p.diff+0.01)))
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

    ## preparing size of output vector
    if (size==0) {
        n <- mongo.count(mongo, ns=set, query=query)
        if (n==0 | n==-1){
            stop("Zero tweets match the specified conditions. Is the DB name correct?")
        }
    }
    if (size>0 & size<1) {
        n <- ceiling(size * mongo.count(mongo, ns=set, query=query))}
    if (size>=1) {n <- size}
    out <- rep(NA, n)

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
    res <- mongo.find(mongo=mongo, ns=set, query=query, fields=fields)
    i <- 1
    if (verbose==TRUE) {pb <- txtProgressBar(min=1,max=n, style=3)}
    if (size < 1){
        while (mongo.cursor.next(res)) {
            out[i] <- list(mongo.bson.to.list(mongo.cursor.value(res)))
            i <- i + 1
            if (verbose==TRUE) {setTxtProgressBar(pb, i)}
        }
    }
    if (size >= 1){
        while (mongo.cursor.next(res) & i <= size) {
            out[i] <- list(mongo.bson.to.list(mongo.cursor.value(res)))
            i <- i + 1
            if (verbose==TRUE) {setTxtProgressBar(pb, i)}
        }        
    }
    out <- parseMongo(out, fields=fields.arg)
    class(out) <- "tweets"
    return(out)

}


unlistWithNA <- function(lst, field){
    if (length(field)==1){
        notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field]])))
        vect <- rep(NA, length(lst))
        vect[notnulls] <- unlist(lapply(lst[notnulls], '[[', field))
    }
    if (length(field)==2){
        notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field[1]]][[field[2]]])))
        vect <- rep(NA, length(lst))
        vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]]))
    }
    if (length(field)==3 & field[1]!="geo"){
        notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field[1]]][[field[2]]][[field[3]]])))
        vect <- rep(NA, length(lst))
        vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]][[field[3]]]))
    }
    if (field[1]=="geo"){
        notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field[1]]][[field[2]]])))
        vect <- rep(NA, length(lst))
        vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]][[as.numeric(field[3])]]))
    }

    if (length(field)==4 && field[2]!="urls"){
        notnulls <- unlist(lapply(lst, function(x) length(x[[field[1]]][[field[2]]][[field[3]]][[field[4]]])>0))
        vect <- rep(NA, length(lst))
        vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]][[field[3]]][[field[4]]]))
    }
    if (length(field)==4 && field[2]=="urls"){
        notnulls <- unlist(lapply(lst, function(x) length(x[[field[1]]][[field[2]]])>0))
        vect <- rep(NA, length(lst))
        vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]][[as.numeric(field[3])]][[field[4]]]))
    }
    if (length(field)==6 && field[2]=="bounding_box"){
        notnulls <- unlist(lapply(lst, function(x) length(x[[field[1]]][[field[2]]])>0))
        vect <- rep(NA, length(lst))
        vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) 
            x[[field[1]]][[field[2]]][[field[3]]][[as.numeric(field[4])]][[as.numeric(field[5])]][[as.numeric(field[6])]]))
    }
    return(vect)
}

parseMongo <- function(tweets, fields){
    fields.list <- strsplit(fields, "\\.")
    tweets.out <- matrix(NA, 
        nrow=length(tweets), ncol=length(fields))
    for (i in 1:length(fields)){
        tweets.out[,i] <- unlistWithNA(tweets, fields.list[[i]])
    }
    tweets.out <- data.frame(tweets.out, stringsAsFactors=F)
    names(tweets.out) <- fields
    return(tweets.out)
}


