#' @rdname getUsersBatch
#' @export
#'
#' @title
#' Returns user data for a vector of Twitter user IDs or screen_names
#'
#' @description
#' \code{getUsersBatch} connects to the REST API of Twitter and returns user
#' objects (user information) for Twitter users, based on their
#' screen names or user IDs
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#'
#' @param screen_names user names of the Twitter users
#'
#' @param ids ids of Twitter users
#'
#' @param include_entities if "true", returned data will include most
#' recent tweet
#'
#' @param oauth_folder folder where OAuth tokens are stored. It can also be the name
#' of a file that contains the token, or a csv file with the format: consumer_key, 
#' consumer_secret,access_token,access_token_secret.
#'
#' @param verbose shows additional ouput about token usage in console
#'
#' @param output If not \code{NULL}, will write user data in raw JSON format
#' to that file
#'
#'
#' @examples \dontrun{
#' ## Download user data for users "p_barbera" and "barackobama"
#'  userdata <- getUsersBatch(screen_names=c("p_barbera", "BarackObama"),
#'    oauth_folder="~/Dropbox/credentials")
#' }
#'

getUsersBatch <- function(ids=NULL, screen_names=NULL, oauth_folder, include_entities="false",
                          verbose=TRUE, output=NULL){

  left.ids <- if (is.null(ids)) {screen_names} else {ids}
  if (!is.null(output)){ conn = file(output, 'w')}
  users.df <- list()
  i <- 1
  while (length(left.ids)>0){
    message(i, "--", length(left.ids), ' users left')
    ids.tmp <- sample(left.ids, min(c(100, length(left.ids))))

    if (!is.null(ids)){
      error <- tryCatch(tmp <- getUsers( oauth_folder, id = ids.tmp, include_entities=include_entities),
                        error = function(e) e)
    }
    if (!is.null(screen_names)){
      error <- tryCatch(tmp <- getUsers( oauth_folder,
                                         screen_names = ids.tmp, include_entities=include_entities),
                        error = function(e) e)
    }
    # if error is found, go to next loop iteration
    if (inherits(error, 'error')){ next }

    if (!is.null(output)){ out <- lapply(tmp, function(x) writeLines(jsonlite::toJSON(x), con=conn)) }

    users.df[[i]] <- data.frame(
      id_str = unlistWithNA(tmp, 'id_str'),
      screen_name = unlistWithNA(tmp, 'screen_name'),
      name = unlistWithNA(tmp, 'name'),
      description = unlistWithNA(tmp, 'description'),
      followers_count = unlistWithNA(tmp, 'followers_count'),
      statuses_count = unlistWithNA(tmp, 'statuses_count'),
      friends_count = unlistWithNA(tmp, 'friends_count'),
      created_at = unlistWithNA(tmp, 'created_at'),
      location = unlistWithNA(tmp, 'location'),
      lang = unlistWithNA(tmp, 'lang'),
      time_zone = unlistWithNA(tmp, 'time_zone'),
      status.id_str = unlistWithNA(tmp, c('status', 'id_str')),
      status.created_at = unlistWithNA(tmp, c('status', 'created_at')),
      status.text = unlistWithNA(tmp, c('status', 'text')),
      stringsAsFactors=F)

    i <- i + 1
    left.ids <- left.ids[left.ids %in% ids.tmp == FALSE]
  }
  users.df <- do.call(rbind, users.df)
  if (!is.null(output)){ close(conn) }
  return(users.df)
}
