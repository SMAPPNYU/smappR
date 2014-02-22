\name{getUsers}
\alias{getUsers}
\title{Returns user data for up to 100 Twitter users}
\usage{
  getUsers(oauth_folder = "~/credentials",
    screen_names = NULL, id = NULL,
    include_entities = "true")
}
\arguments{
  \item{screen_name}{user names of the Twitter users}

  \item{id}{ids of Twitter users}

  \item{include_entities}{if "true", returned data will
  include most recent tweet}

  \item{oauth_folder}{folder where OAuth tokens are
  stored.}
}
\description{
  \code{getUsers} connects to the REST API of Twitter and
  returns user objects (user information) for up to 100
  Twitter users, based on their screen names or user IDs
}
\examples{
\dontrun{
## Download user data for user "p_barbera"
 userdata <- getUsers(screen_name="p_barbera", oauth_folder="oauth")
}
}
\author{
  Pablo Barbera \email{pablo.barbera@nyu.edu}
}

