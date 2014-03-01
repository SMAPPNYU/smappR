#' @rdname getTimeline
#' @export
#'
#' @title 
#' Returns up to 3,200 recent tweets from a given user
#'
#' @description
#' \code{getTimeline} connects to the REST API of Twitter and returns up to
#' 3,200 recent tweets sent by these user.
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#'
#' @param filename file where tweets will be stored (in json format)
#'
#' @param n number of tweets to be downloaded (maximum is 3,200)
#'
#' @param screen_name user name of the Twitter user for which his/her tweets
#' will be downloaded
#' 
#' @param id id of Twitter user for which his/her tweets will be downloaded
#' (Use either of these two arguments)
#'
#' @param oauth_folder folder where OAuth tokens are stored.
#'
#' @param since_id id of the oldest tweet to be downloaded. Useful if, for 
#' example, we're only interested in getting tweets sent after a certain
#' date.
#'
#' @param trim_user if "true", downloaded tweets will include user object
#' embedded. If "false", only tweet information will be downloaded.
#'
#' @param cursor See \url{https://dev.twitter.com/docs/api/1.1/get/statuses/user_timeline}
#'
#' @examples \dontrun{
#' ## Download recent tweets by user "p_barbera"
#'  friends <- getTimeline(screen_name="p_barbera", oauth_folder="oauth")
#' }
#'

getTimeline <- function(filename, n=3200, oauth_folder="~/credentials", screen_name=NULL, 
    id=NULL, since_id=-1, trim_user="true"){

    require(rjson); require(ROAuth)

    ## create list of credentials
    creds <- list.files(oauth_folder, full.names=T)
    ## open a random credential
    cr <- sample(creds, 1)
    cat(cr, "\n")
    load(cr)
    ## while rate limit is 0, open a new one
    limit <- getLimitTimeline(my_oauth)
    cat(limit, " hits left\n")
    while (limit==0){
        cr <- sample(creds, 1)
        cat(cr, "\n")
        load(cr)
        Sys.sleep(1)
        # sleep for 5 minutes if limit rate is less than 100
        rate.limit <- getLimitRate(my_oauth)
        if (rate.limit<100){
            Sys.sleep(300)
        }
        limit <- getLimitTimeline(my_oauth)
        cat(limit, " hits left\n")
    }
    ## url to call
    url <- "https://api.twitter.com/1.1/statuses/user_timeline.json"

    ## first API call
    if (!is.null(screen_name)){
        params <- list(screen_name = screen_name, count=200, trim_user=trim_user)
    }
    if (!is.null(id)){
        params <- list(id=id, count=200, trim_user=trim_user)   
    }
    if (!is.null(since_id)){
        params[["since_id"]] <- since_id
    }
    
    url.data <- my_oauth$OAuthRequest(URL=url, params=params, method="GET", 
    cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl")) 
    Sys.sleep(.5)
    ## one API call less
    limit <- limit - 1
    ## changing oauth token if we hit the limit
    cat(limit, " hits left\n")
    while (limit==0){
        cr <- sample(creds, 1)
        cat(cr, "\n")
        load(cr)
        Sys.sleep(1)
        # sleep for 5 minutes if limit rate is less than 100
        rate.limit <- getLimitRate(my_oauth)
        if (rate.limit<100){
            Sys.sleep(300)
        }
        limit <- getLimitTimeline(my_oauth)
        cat(limit, " hits left\n")
    }
    ## trying to parse JSON data
    json.data <- fromJSON(url.data, unexpected.escape = "skip")
    if (length(json.data$error)!=0){
        cat(url.data)
        stop("error! Last cursor: ", cursor)
    }
    ## writing to disk
    conn <- file(filename, "a")
    invisible(lapply(json.data, function(x) writeLines(toJSON(x), con=conn)))
    close(conn)
    ## max_id
    tweets <- length(json.data)
    max_id <- json.data[[tweets]]$id_str
    cat(tweets, "tweets. Max id: ", max_id, "\n")
    max_id_old <- "none"

    while (tweets < n & max_id != max_id_old & 
        as.numeric(max_id) > as.numeric(since_id)){
        max_id_old <- max_id
        if (!is.null(screen_name)){
            params <- list(screen_name = screen_name, count=200, max_id=max_id,
                trim_user=trim_user)
        }
        if (!is.null(id)){
            params <- list(id=id, count=200, max_id=max_id, trim_user=trim_user)
        }
        if (!is.null(since_id)){
            params[['since_id']] <- since_id
        }
        url.data <- my_oauth$OAuthRequest(URL=url, params=params, method="GET", 
        cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl")) 
        Sys.sleep(1)
        ## one API call less
        limit <- limit - 1
        ## changing oauth token if we hit the limit
        cat(limit, " hits left\n")
        while (limit==0){
            cr <- sample(creds, 1)
            cat(cr, "\n")
            load(cr)
            Sys.sleep(1)
            # sleep for 5 minutes if limit rate is less than 100
            rate.limit <- getLimitRate(my_oauth)
            if (rate.limit<100){
                Sys.sleep(300)
            }
            limit <- getLimitTimeline(my_oauth)
            cat(limit, " hits left\n")
        }
        ## trying to parse JSON data
        json.data <- fromJSON(url.data, unexpected.escape = "skip")
        if (length(json.data$error)!=0){
            cat(url.data)
            stop("error! Last cursor: ", cursor)
        }
        ## writing to disk
        conn <- file(filename, "a")
        invisible(lapply(json.data, function(x) writeLines(toJSON(x), con=conn)))
        close(conn)
        ## max_id
        tweets <- tweets + length(json.data)
        max_id <- json.data[[length(json.data)]]$id_str
        cat(tweets, "tweets. Max id: ", max_id, "\n")
    }
}


getLimitTimeline <- function(my_oauth){
    require(rjson); require(ROAuth)
    url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
    params <- list(resources = "statuses,application")
    response <- my_oauth$OAuthRequest(URL=url, params=params, method="GET", 
        cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
    return(unlist(fromJSON(response)$resources$statuses$`/statuses/user_timeline`[['remaining']]))

}






