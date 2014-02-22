#' @rdname getUsers
#' @export
#'
#' @title 
#' Returns user data for up to 100 Twitter users
#'
#' @description
#' \code{getUsers} connects to the REST API of Twitter and returns user
#' objects (user information) for up to 100 Twitter users, based on their
#' screen names or user IDs
#' 
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#'
#' @param screen_name user names of the Twitter users
#' 
#' @param id ids of Twitter users
#'
#' @param include_entities if "true", returned data will include most
#' recent tweet
#'
#' @param oauth_folder folder where OAuth tokens are stored.
#'
#'
#' @examples \dontrun{
#' ## Download user data for user "p_barbera"
#'  userdata <- getUsers(screen_name="p_barbera", oauth_folder="oauth")
#' }
#'

getUsers <- function(oauth_folder="~/credentials", screen_names=NULL, 
    id=NULL, include_entities="true"){

    require(rjson); require(ROAuth)

    ## create list of credentials
    creds <- list.files(oauth_folder, full.names=T)
    ## open a random credential
    cr <- sample(creds, 1)
    cat(cr, "\n")
    load(cr)
    ## while rate limit is 0, open a new one
    limit <- getLimitUsers(my_oauth)
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
        limit <- getLimitUsers(my_oauth)
        cat(limit, " hits left\n")
    }
    ## url to call
    url <- "https://api.twitter.com/1.1/users/lookup.json"

    ## first API call
    if (!is.null(screen_name)){
        screen_names <- paste(screen_names, collapse=",")
        params <- list(screen_name = screen_name, include_entities=include_entities)
    }
    if (!is.null(id)){
        ids <- paste(id, collapse=",")
        params <- list(user_id=ids, include_entities=include_entities)   
    }
    
    url.data <- my_oauth$OAuthRequest(URL=url, params=params, method="GET", 
    cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl")) 
    Sys.sleep(.5)
    return(fromJSON(url.data))
}


getLimitUsers <- function(my_oauth){
    require(rjson); require(ROAuth)
    url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
    params <- list(resources = "users,application")
    response <- my_oauth$OAuthRequest(URL=url, params=params, method="GET", 
        cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
    return(unlist(fromJSON(response)$resources$users$`/users/lookup`[['remaining']]))

}






