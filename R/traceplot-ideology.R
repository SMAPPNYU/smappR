#' @rdname traceplot.ideology
#' @export
#'
#' @title 
#' Displays trace plot for MCMC chains of ideology estimator
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#'
#'
#' @param results ideology estimation object from \code{estimate.ideology} function
#' 
#' @param par parameter for which trace plot is to be displayed
#'
#'
#' @examples \dontrun{
#' ## download list of friends for a given user
#'  friends <- getFriends(screen_name = "p_barbera", oauth_folder="oauth")
#' ## estimating ideology 
#'  results <- estimate.ideology(friends)
#' ## trace plot
#'  traceplot.ideology(results, "theta")
#' }
#'

traceplot.ideology <- function(results, par="theta"){
    iters <- dim(results$samples)[[1]]
    par(mar=c(3, 3, 2, 3))
    plot(1:iters, results$samples[,1,par], type="l", col="red",
        ylim=range(results$samples[,,par]))
    mtext("Iteration", side=1, line=2)
    mtext(par, side=2, line=2)
    lines(1:iters, results$samples[,2,par], col="blue")

}

