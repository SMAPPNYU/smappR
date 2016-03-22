#' @rdname getRetweets
#' @export
#'
#' @title 
#' Returns the list of user IDs that retweeted a specific tweet.
#'
#' @description
#' \code{getRetweets} connects to the REST API of Twitter and returns a
#' list of up to 100 user IDs belonging to users who have retweeted the 
#' tweet specified by the id parameter.
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#'
#' @param id The numerical ID of the desired status
#'
#' @param oauth_folder folder where OAuth tokens are stored.
#'
#' @param cursor See \url{https://dev.twitter.com/docs/api/1.1/get/statuses/retweeters/ids}
#'
#' @param verbose If \code{TRUE}, prints information about API calls on console
#'
#' @param sleep Number of seconds to sleep between API calls.
#'
#' @examples \dontrun{
#' ## Download list of recent user IDs retweeting a tweet by Hillary Clinton
#'  rts <- getRetweets(id='653733796408377344', oauth_folder="~/credentials/twitter")
#' }
#'

getRetweets <- function(id=NULL, oauth_folder, cursor=-1, verbose=TRUE, sleep=1){

    ## create list of credentials
    creds <- list.files(oauth_folder, full.names=T)
    ## open a random credential
    cr <- sample(creds, 1)
    if (verbose) {message(cr, "\n")}
    load(cr)
    ## while rate limit is 0, open a new one
    limit <- getLimitRetweets(my_oauth)
    if (verbose) {message(limit, " API calls left")}
    while (limit==0){
        cr <- sample(creds, 1)
        if (verbose){message(cr, "\n")}
        load(cr)
        Sys.sleep(sleep)
        # sleep for 5 minutes if limit rate is less than 100
        rate.limit <- getLimitRate(my_oauth)
        if (rate.limit<100){
            Sys.sleep(300)
        }
        limit <- getLimitRetweets(my_oauth)
        if (verbose){message(limit, " API calls left")}
    }
    ## url to call
    url <- "https://api.twitter.com/1.1/statuses/retweeters/ids.json"
    ## empty list for retweeters
    retweeters <- c()
    ## while there's more data to download...
    while (cursor!=0){
        ## making API call
        params <- list(id = id, stringify_ids="true")
        url.data <- my_oauth$OAuthRequest(URL=url, params=params, method="GET", 
            cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
        Sys.sleep(sleep)
        ## one API call less
        limit <- limit - 1
        ## trying to parse JSON data
        json.data <- rjson::fromJSON(url.data)
        if (length(json.data$error)!=0){
            if(verbose){message(url.data)}
            stop("error! Last cursor: ", cursor)
        }
        ## adding new IDS
        retweeters <- c(retweeters, as.character(json.data$ids))

        ## previous cursor
        prev_cursor <- json.data$previous_cursor_str
        ## next cursor
        cursor <- json.data$next_cursor_str
        ## giving info
        message(length(retweeters), " retweeters. Next cursor: ", cursor)

        ## changing oauth token if we hit the limit
        if (verbose){message(limit, " API calls left")}
        while (limit==0){
            cr <- sample(creds, 1)
            if (verbose){message(cr, "\n")}
            load(cr)
            Sys.sleep(sleep)
            # sleep for 5 minutes if limit rate is less than 100
            rate.limit <- getLimitRate(my_oauth)
            if (rate.limit<100){
                Sys.sleep(300)
            }
            limit <- getLimitRetweets(my_oauth)
            if (verbose){message(limit, " API calls left")}
        }
    }
    return(retweeters)
}

getLimitRetweets <- function(my_oauth){
    url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
    params <- list(resources = "statuses,application")
    response <- my_oauth$OAuthRequest(URL=url, params=params, method="GET", 
        cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
    return(unlist(rjson::fromJSON(response)$resources$statuses$`/statuses/retweeters/ids`[['remaining']]))
}
