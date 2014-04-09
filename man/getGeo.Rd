\name{getGeo}
\alias{getGeo}
\title{Returns geographic information about a location string}
\usage{
getGeo(location, verbose = FALSE,
  rdstk = "http://www.datasciencetoolkit.org")
}
\arguments{
  \item{location}{location string for which information is
  desired}

  \item{verbose}{If TRUE, provides additional information
  on console.}
}
\description{
\code{getGeo} connects to the Data Science Tool Kit and
converts the location string into a pair of coordinates,
and then into information (city, state, country,
congressional district...) for those coordinates
}
\examples{
\dontrun{
## Download geographic information for "New York"
 getGeo("New York")
}
}
\author{
Pablo Barbera \email{pablo.barbera@nyu.edu}
}

