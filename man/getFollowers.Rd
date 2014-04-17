\name{getFollowers}
\alias{getFollowers}
\title{Returns the list of user IDs that correspond to a given user's follower}
\usage{
getFollowers(screen_name, oauth_folder, cursor = -1, user_id = NULL,
  verbose = TRUE)
}
\arguments{
  \item{screen_name}{user name of the Twitter user for
  which their followers will be downloaded}

  \item{oauth_folder}{folder where OAuth tokens are
  stored.}

  \item{cursor}{See
  \url{https://dev.twitter.com/docs/api/1.1/get/followers/ids}}

  \item{user_id}{user id of the Twitter user for which
  their friends will be downloaded}

  \item{verbose}{If \code{TRUE}, prints information about
  API calls on console}
}
\description{
\code{getFollowers} connects to the REST API of Twitter and
returns the list of followers of a given user. Note that
this function allows the use of multiple OAuth token to
make the process more efficient.
}
\examples{
\dontrun{
## Download list of followers of user "p_barbera"
 followers <- getFollowers(screen_name="p_barbera", oauth_folder="oauth")
}
}
\author{
Pablo Barbera \email{pablo.barbera@nyu.edu}
}

