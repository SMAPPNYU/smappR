#' @rdname tweetsToDF
#' @export
#'
#' @title 
#' Converts object of class "tweets" to data frame
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#'
#' @param tweets object of class tweets
#'
#'

tweetsToDF <- function(tweets){
    class(tweets) <- "data.frame"
    return(tweets)
}

