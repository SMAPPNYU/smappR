#' @rdname getList
#' @export
#'
#' @title 
#' Returns the list of users added to a Twitter list
#'
#' @description
#' \code{getList} connects to the REST API of Twitter and returns a
#' data frame with information about users included in a Twitter list
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#'
#' @param list_name name of the list
#'
#' @param screen_name user that created the list
#'
#' @param oauth_folder folder where OAuth tokens are stored.
#'
#' @param cursor See \url{https://dev.twitter.com/docs/api/1.1/get/lists/members}
#'
#'
#' @examples \dontrun{
#' ## Download Twitter list of "official-twitter-accts" created by @@twitter
#'  accts <- getList(list_name="official-twitter-accts" screen_name="twitter", 
#'    oauth_folder="oauth")
#' }
#'

getList <- function(list_name, screen_name, oauth_folder, cursor=-1){

    ## create list of credentials
    creds <- list.files(oauth_folder, full.names=T)
    ## open a random credential
    cr <- sample(creds, 1)
    message(cr, "\n")
    load(cr)
    ## while rate limit is 0, open a new one
    limit <- getLimitList(my_oauth)
    message(limit, " API calls left\n")
    while (limit==0){
        cr <- sample(creds, 1)
        message(cr, "\n")
        load(cr)
        Sys.sleep(1)
        # sleep for 5 minutes if limit rate is less than 100
        rate.limit <- getLimitRate(my_oauth)
        if (rate.limit<100){
            Sys.sleep(300)
        }
        limit <- getLimitList(my_oauth)
        message(limit, " API calls left\n")
    }
    ## url to call
    url <- "https://api.twitter.com/1.1/lists/members.json"
    ## empty list for members
    members <- list()
    ## while there's more data to download...
    while (cursor!=0){
        ## making API call
        params <- list(slug=list_name, owner_screen_name=screen_name,
            include_entities='true', cursor=cursor, skip_status='true')
        url.data <- my_oauth$OAuthRequest(URL=url, params=params, method="GET", 
        cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
        Sys.sleep(1)
        ## one API call less
        limit <- limit - 1
        ## trying to parse JSON data
        json.data <- rjson::fromJSON(url.data, unexpected.escape = "skip")
        if (length(json.data$error)!=0){
            message(url.data)
            stop("error! Last cursor: ", cursor)
        }
        ## transforming to DF and storing in list
        members[[length(members)+1]] <- userDataToDF(json.data$users)

        ## previous cursor
        prev_cursor <- json.data$previous_cursor_str
        ## next cursor
        cursor <- json.data$next_cursor_str
        ## giving info
        message(sum(unlist(lapply(members, nrow))), 
            " users in list. Next cursor: ", cursor, "\n")

        ## changing oauth token if we hit the limit
        message(limit, " API calls left\n")
        while (limit==0){
            cr <- sample(creds, 1)
            message(cr, "\n")
            load(cr)
            Sys.sleep(1)
            # sleep for 5 minutes if limit rate is less than 100
            rate.limit <- getLimitRate(my_oauth)
            if (rate.limit<100){
                Sys.sleep(300)
            }
            limit <- getLimitList(my_oauth)
            message(limit, " API calls left\n")
        }
    }
    members <- do.call(rbind, members)
    return(members)
}

getLimitList <- function(my_oauth){
    url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
    params <- list(resources = "lists,application")
    response <- my_oauth$OAuthRequest(URL=url, params=params, method="GET", 
        cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
    return(unlist(rjson::fromJSON(response)$resources$lists$`/lists/members`['remaining']))
}


userDataToDF <- function(json){
    df <- data.frame(
        id = unlistWithNA(json, 'id'),
        id_str = unlistWithNA(json, 'id_str'),
        name = unlistWithNA(json, 'name'),
        screen_name = unlistWithNA(json, 'screen_name'),
        location = unlistWithNA(json, 'location'),
        description = unlistWithNA(json, 'description'),
        url = unlistWithNA(json, 'url'),
        followers_count = unlistWithNA(json, 'followers_count'),
        friends_count = unlistWithNA(json, 'friends_count'),
        created_at = unlistWithNA(json, 'created_at'),
        time_zone = unlistWithNA(json, 'time_zone'),
        lang = unlistWithNA(json, 'lang'),
        stringsAsFactors=F)

    return(df)
}



