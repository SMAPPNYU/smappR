\name{traceplot}
\alias{traceplot}
\title{Displays trace plot for MCMC chains of ideology estimator}
\usage{
  traceplot(results, par = "theta")
}
\arguments{
  \item{results}{ideology estimation object from
  \code{estimate.ideology} function}

  \item{par}{parameter for which trace plot is to be
  displayed}
}
\description{
  Displays trace plot for MCMC chains of ideology estimator
}
\examples{
\dontrun{
## download list of friends for a given user
 friends <- getFriends(screen_name = "p_barbera", oauth_folder="oauth")
## estimating ideology
 results <- estimate.ideology(friends)
## trace plot
 traceplot(results, "theta")
}
}
\author{
  Pablo Barbera \email{pablo.barbera@nyu.edu}
}

