#' @rdname plot.tweets
#' @export
#'
#' @title 
#' Plots number of tweets by hour or minute
#' @description
#' Plots number of tweets by hour or minute
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}, Megan Metzger \email{megan.metzger@@nyu.edu}
#'
#' @param tweets object of class tweets
#'
#' @param breaks either "minutes" or "hours"
#'
#' @param return_plot If \code{TRUE}, function returns plot as an object (useful
#' when saving to a file.)
#'
#' @param missing_minutes If \code{TRUE}, minutes with zero tweets in the plot
#' where breaks is minutes will be interpolated.
#'
#' @param hours numeric. When different from zero, will add that number of hours
#' to the timestamp of the tweets. That is, when hours is zero, then all times
#' are expressed in NY time.
#'
#' @examples \dontrun{
#' ## connect to the Mongo database
#'  mongo <- mongo.create("SMAPP_HOST:PORT", db="DATABASE")
#'  mongo.authenticate(mongo, username="USERNAME", password="PASSWORD", db="DATABASE")
#'  set <- "DATABASE.COLLECTION"
#'
#' ## extract all tweets that mention 'occupygezi' and do a quick plot
#'  tweets <- extract.tweets(set, string="occupygezi", fields="created_at")
#'  plot(tweets)
#' }
#'

plot.tweets <- function(tweets, breaks='hours', return_plot=FALSE, missing_minutes=FALSE, hours=0){
    if ('created_at' %in% names(tweets) == FALSE){
        stop("Tweets do not contain 'created_at' field. I don't know how to plot tweets without that field!")
    }
    require(ggplot2); require(scales)
    dates <- tweets$created_at
    dates <- formatTwDate(dates) +60*60*hours
    if (breaks=='minutes'){
        mins <- substr(as.character(dates), 1, 16)
        n.tweets <- table(mins)
        mins.x <- seq(as.POSIXct(paste(names(n.tweets)[1], ':00', sep="")), 
            as.POSIXct(paste(names(n.tweets)[length(n.tweets)], ':00', sep="")),
            by=60)
        mins.x.text <- substr(as.character(mins.x), 1, 16)
        tweets.df <- data.frame(minute = mins.x, 
            tweets = 0, stringsAsFactors=F)
        tweets.df$tweets[mins.x.text %in% names(n.tweets)] <- as.numeric(n.tweets)
        if (missing_minutes==TRUE){
          missing <- which(substr(as.character(tweets.df$minute), 15, 16) %in% c("00", "30"))
          missing <- unique(c(missing, which(tweets.df$tweets<10)))
          tweets.df <- tweets.df[-missing,]      
        }

        p <- ggplot(tweets.df, aes(x=minute, y=tweets))
        pq <- pq <- p + geom_line() +
            scale_x_datetime("EST Time") +
            theme_bw() +
             scale_y_continuous(name="Tweets Per Minute", expand = c(0, 0),
                limits=c(0, max(tweets.df$tweets)), label=comma) +
            theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())
        print(pq)
        if (return_plot) return(pq)
    }
    if (breaks=='hours'){
        hours <- substr(as.character(dates), 1, 13)
        n.tweets <- table(hours)
        hours.x <- seq(as.POSIXct(paste(names(n.tweets)[1], ':00:00', sep="")), 
            as.POSIXct(paste(names(n.tweets)[length(n.tweets)], ':00:00', sep="")),
            by=3600)
        hours.x.text <- substr(as.character(hours.x), 1, 13)
        tweets.df <- data.frame(hour = hours.x, 
            tweets = 0, stringsAsFactors=F)
        tweets.df$tweets[hours.x.text %in% names(n.tweets)] <- as.numeric(n.tweets)
        if (tweets.df$tweets[length(tweets.df$tweets)]==0){
            tweets.df <- tweets.df[-length(tweets.df$tweets),]
        }
        p <- ggplot(tweets.df, aes(x=hour, y=tweets))
        pq <- pq <- p + geom_line() +
            scale_x_datetime("EST Time") +
            theme_bw() +
             scale_y_continuous(name="Tweets Per Hour", expand = c(0, 0),
                limits=c(0, max(tweets.df$tweets)), label=comma) +
            theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())
        print(pq)
        if (return_plot) return(pq)
    }
    if (breaks=='days'){
        days <- substr(as.character(dates), 1, 10)
        n.tweets <- table(days)
        days.x <- seq(as.Date(names(n.tweets)[1]), 
            as.Date(names(n.tweets)[length(n.tweets)]), by=1)
        days.x.text <- as.character(days.x)
        tweets.df <- data.frame(day = days.x, 
            tweets = 0, stringsAsFactors=F)
        tweets.df$tweets[days.x.text %in% names(n.tweets)] <- as.numeric(n.tweets)
        if (tweets.df$tweets[length(tweets.df$tweets)]==0){
            tweets.df <- tweets.df[-length(tweets.df$tweets),]
        }
        p <- ggplot(tweets.df, aes(x=day, y=tweets))
        pq <- pq <- p + geom_line() +
            theme_bw() +
             scale_y_continuous(name="Tweets Per Day", expand = c(0, 0),
                limits=c(0, max(tweets.df$tweets)), label=comma) +
            theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.title.x = element_blank())
        print(pq)
        if (return_plot) return(pq)
    }

}

