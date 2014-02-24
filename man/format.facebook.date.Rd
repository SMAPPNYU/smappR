\name{format.facebook.date}
\alias{format.facebook.date}
\title{Converts from Facebook date format to R date format}
\usage{
  format.facebook.date(datestring, format = "datetime")
}
\arguments{
  \item{datestring}{Date string in Facebook format}

  \item{output}{Either "date", for Date format; or
  "datetime" for POSIX.}
}
\description{
  Converts from Facebook date format to R date format
}
\examples{
\dontrun{
## example of Facebook data
  example <- "2014-02-21T16:22:03+0000"
  dt <- format.facebook.date(example)
}
}
\author{
  Pablo Barbera \email{pablo.barbera@nyu.edu}
}

