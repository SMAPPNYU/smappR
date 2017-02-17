#' @rdname getStatuses
#' @export
#'
#' @title 
#' Downloads tweets by their ID from REST API and saves to a json file
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#'
#' @param ids list of tweet IDs to be downloaded
#'
#' @param filename Name of file where json tweets will be stored
#'
#' @param oauth_folder folder where OAuth tokens are stored.
#'
#' @param verbose If \code{TRUE}, prints information about API calls on console
#'
#' @param sleep Number of seconds to sleep between API calls.
#'
#'

getStatuses <- function(ids=NULL, filename, oauth_folder, verbose=TRUE, sleep=1){

    ## create list of credentials
    creds <- list.files(oauth_folder, full.names=T)
    ## open a random credential
    cr <- sample(creds, 1)
    if (verbose) {message(cr, "\n")}
    load(cr)
    ## while rate limit is 0, open a new one
    limit <- getLimitStatuses(my_oauth)
    if (verbose) {message(limit, " API calls left\n")}
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
        limit <- getLimitStatuses(my_oauth)
        if (verbose){message(limit, " API calls left\n")}
    }
    ## url to call
    url <- "https://api.twitter.com/1.1/statuses/lookup.json"
    ids.left <- ids

    # preparing OAuth token for httr
    options("httr_oauth_cache"=FALSE)
    app <- httr::oauth_app("twitter", key = my_oauth$consumerKey, 
        secret = my_oauth$consumerSecret)
    credentials <- list(oauth_token = my_oauth$oauthKey, oauth_token_secret = my_oauth$oauthSecret)
    twitter_token <- httr::Token1.0$new(endpoint = NULL, params = list(as_header = TRUE), 
        app = app, credentials = credentials)

    ## while there's more data to download...
    while (length(ids.left)>0){
        ## making API call
        query <- list(id = paste(ids.left[1:100], collapse=","))
        url.data <- httr::GET(url, query = query, httr::config(token = twitter_token))
        Sys.sleep(sleep)
        ## one API call less
        limit <- limit - 1
        
        # parsing JSON
        json.data <- httr::content(url.data)
        if (length(json.data$error)!=0){
            message(url.data)
            stop("error downloading IDs! First ID not downloaded", ids[1])
        }

        ## writing to disk
        conn <- file(filename, "a")
        invisible(lapply(json.data, function(x) writeLines(jsonlite::toJSON(x, null="null"), con=conn, useBytes=TRUE)))
        close(conn)
    
        # removing IDs done
        ids.left <- ids.left[-(1:100)]

        ## changing oauth token if we hit the limit
        if (verbose){message(limit, " API calls left\n")}
        cr_old <- cr
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
            limit <- getLimitStatuses(my_oauth)
            if (verbose){message(limit, " API calls left\n")}
        }
        if (cr != cr_old) {
            app <- httr::oauth_app("twitter", key = my_oauth$consumerKey, 
                secret = my_oauth$consumerSecret)
            credentials <- list(oauth_token = my_oauth$oauthKey, oauth_token_secret = my_oauth$oauthSecret)
            twitter_token <- httr::Token1.0$new(endpoint = NULL, params = list(as_header = TRUE), 
                app = app, credentials = credentials)
        }
    }
}

getLimitRate <- function(my_oauth){
    url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
    params <- list(resources = "followers,application")
    response <- my_oauth$OAuthRequest(URL=url, params=params, method="GET", 
        cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
    return(unlist(rjson::fromJSON(response)$resources$application$`/application/rate_limit_status`[['remaining']]))
}



getLimitStatuses <- function(my_oauth){
    url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
    params <- list(resources = "statuses,application")
    response <- my_oauth$OAuthRequest(URL=url, params=params, method="GET", 
        cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
    return(unlist(rjson::fromJSON(response)$resources$statuses$`/statuses/lookup`[['remaining']]))

}
