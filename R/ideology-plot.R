#' @rdname ideology.plot
#' @export
#'
#' @title 
#' Displays estimated ideology with other reference ideal points
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#'
#' @param results ideology estimation object from \code{estimate.ideology} function
#' 
#'
#' @examples \dontrun{
#' ## download list of friends for a given user
#'  friends <- getFriends(screen_name = "p_barbera", oauth_folder="oauth")
#' ## estimating ideology 
#'  results <- estimate.ideology(friends)
#' ## trace plot
#'  traceplot.ideology(results, "theta")
#' ## ideology plot
#'  ideology.plot(results)
#' }
#'

ideology.plot <- function(results){
    require(ggplot2)
    # loading reference data
    data(refdata)
    data <- refdata
    # computing credible interval for user
    theta.lo <- quantile(results$samples[,,2], .025)
    theta <- mean(results$samples[,,2])
    theta.hi <- quantile(results$samples[,,2], .975)
    data <- rbind(data, c(user, theta, theta.lo, theta.hi))
    data$phi <- as.numeric(data$phi)
    data$phi.lo <- as.numeric(data$phi.lo)
    data$phi.hi <- as.numeric(data$phi.hi)

    p <- ggplot(data, aes(y=reorder(screenName, -phi), x=phi))
    pq <- p + geom_point(size=1.25) + 
        geom_segment(width=.5, aes(x=phi.lo, xend=phi.hi, y=reorder(screenName, -phi), 
            yend=reorder(screenName, -phi)), position=position_dodge(.5)) +
        theme_bw() + scale_y_discrete("") + 
        scale_x_continuous(expression(paste("95% Intervals for ", phi[j], 
            " or ", theta[i], ", Estimated Ideological Ideal Points")), 
         lim=range(data[,2:4]))
    suppressMessages(suppressWarnings(print(pq)))
}
