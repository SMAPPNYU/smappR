\name{getStatuses}
\alias{getStatuses}
\title{Downloads tweets by their ID from REST API and saves to a json file}
\usage{
getStatuses(ids = NULL, filename, oauth_folder, verbose = TRUE, sleep = 1)
}
\arguments{
  \item{ids}{list of tweet IDs to be downloaded}

  \item{file}{Name of file where json tweets will be
  stored}

  \item{oauth_folder}{folder where OAuth tokens are
  stored.}

  \item{verbose}{If \code{TRUE}, prints information about
  API calls on console}

  \item{sleep}{Number of seconds to sleep between API
  calls.}
}
\description{
Downloads tweets by their ID from REST API and saves to a
json file
}
\author{
Pablo Barbera \email{pablo.barbera@nyu.edu}
}

