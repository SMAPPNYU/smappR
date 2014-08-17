#' @rdname extract.hashtags
#' @export
#'
#' @title 
#' Connect to Mongo database and extract hashtags from each tweet.
#'
#' @description
#' \code{extract.hashtags} opens a connection to the Mongo database in
#' the lab computer and returns a list of hashtags used in all tweets,
#' or in tweets that contain a given keyword. In combination with 
#' \code{summary.retweets}, this is a quick way to display the top
#' hashtags used in a collection of tweets.
#'
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#'
#' @param set string, name of the collection of tweets in the Mongo database to query.
#'
#' @param text vector with tweets text. To be used when all other arguments
#' are \code{NULL}. The function then will extract and count the hashtags in
#' in text.
#'
#' @param string string or vector of strings, set to NULL by default (will 
#' return hashtags for all tweets). If it is a string, it will return all 
#' hashtags that were used in tweets containing that string. If it is a vector
#' of strings, it will return all hashtags that were used in tweets containing
#' at least one of the strings.
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
#'
#' @examples \dontrun{
#' ## connect to the Mongo database
#'  mongo <- mongo.create("SMAPP_HOST:PORT", db="DATABASE")
#'  mongo.authenticate(mongo, username="USERNAME", password="PASSWORD", db="DATABASE")
#'  set <- "DATABASE.COLLECTION"
#'
#' ## extract all hashtags in a collection of tweets
#'  ht <- extract.hashtags(set)
#'
#' ## show top 10 hashtags
#' summary(ht, n=10)
#'
#' ## extract all hashtags that are used in tweets that mention "occupygezi"
#'  ht <- extract.hashtags(set, string="occupygezi")
#'
#' ## show top 10 hashtags in tweets mentioning "occupygezi"
#'  summary(ht, n=10)
#' }
#'
#'

## extract hashtags that appear associated with a string
extract.hashtags <- function(set, text=NULL, string=NULL, from=NULL, to=NULL, verbose=TRUE)
{
    if (is.null(text)){
        query <- list()
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
    

        if (length(query)==0) query <- mongo.bson.empty()
        n <- mongo.count(mongo, ns=set, query=query)
        out <- rep(NA, n)
        # making query
        res <- mongo.find(mongo=mongo, ns=set, query=query, fields=list(entities.hashtags=1L))
        i <- 1
        if (verbose==TRUE) {pb <- txtProgressBar(min=1,max=n, style=3)}
        while (mongo.cursor.next(res)) {
            out[i] <- list(mongo.bson.to.list(mongo.cursor.value(res)))
            i <- i + 1
            if (verbose==TRUE) {setTxtProgressBar(pb, i)}
        }    
        lst <- unlist(lapply(out, "[[", "entities"))
        hashtags <- lst[grep("text", names(lst))]
        names(hashtags) <- NULL
        hashtags <- table(hashtags)
        sorted <- order(hashtags, decreasing=TRUE)
        hashtags <- hashtags[sorted]
        class(hashtags) <- "hashtags"
        return(hashtags)
    }
    if (!is.null(text)){
        hashtags <- regmatches(text,gregexpr("#(\\d|\\w)+",text))
        hashtags <- unlist(hashtags)
        hashtags <- table(hashtags)
        sorted <- order(hashtags, decreasing=TRUE)
        hashtags <- hashtags[sorted]
        class(hashtags) <- "hashtags"
        return(hashtags)
    }
}

#' @export
## summary of retweets
summary.hashtags <- function(hashtags, n=10){
    head(hashtags, n=n)
}



