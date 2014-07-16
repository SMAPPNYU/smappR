#' @rdname formatTwDate
#' @export
#'
#' @title 
#' Converts from Twitter date format to R date format
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#'
#' @param datestring Date string in Twitter format
#'
#' @param output Either "date", for Date format; or "datetime" for POSIX.
#'
#' @examples \dontrun{
#' ## connect to the Mongo database
#'  mongo <- mongo.create("SMAPP_HOST:PORT", db="DATABASE")
#'  mongo.authenticate(mongo, username="USERNAME", password="PASSWORD", db="DATABASE")
#'  set <- "DATABASE.COLLECTION"
#'
#' ## extract all tweets that mention 'occupygezi' and do a quick plot
#'  tweets <- extract.tweets(set, string="occupygezi", fields="created_at")
#'
#' ## convert dates to R format
#'  dates <- formatTwDate(tweets$created_at, format="date")
#'
#' ## see number of tweets per day
#'  table(dates)
#' }
#'

formatTwDate <- function(datestring, format="datetime"){
    if (format=="datetime"){
        date <- as.POSIXct(datestring, format="%a %b %d %H:%M:%S %z %Y")
    }
    if (format=="date"){
        date <- as.Date(datestring, format="%a %b %d %H:%M:%S %z %Y")
    }   
    return(date)
}
