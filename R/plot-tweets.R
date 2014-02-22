#' @rdname plot.tweets
#' @export
#' @method plot tweets
#'
#' @title 
#' Plots number of tweets by hour or minute
#' @description
#' Plots number of tweets by hour or minute
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#'
#' @param tweets object of class tweets
#'
#' @param breaks either "minutes" or "hours"
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

plot.tweets <- function(tweets, breaks='hours'){
    if ('created_at' %in% names(tweets) == FALSE){
        stop("Tweets do not contain 'created_at' field. I don't know how to plot tweets without that field!")
    }
    require(ggplot2)
    dates <- tweets$created_at
    dates <- format.twitter.date(dates)
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

        p <- ggplot(tweets.df, aes(x=minute, y=tweets))
        pq <- pq <- p + geom_line() +
            scale_x_datetime("GMT Time") +
            theme_bw() +
             scale_y_continuous(name="Tweets Per Minute", expand = c(0, 0),
                limits=c(0, max(tweets.df$tweets))) +
            theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())
        print(pq)
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
            scale_x_datetime("GMT Time") +
            theme_bw() +
             scale_y_continuous(name="Tweets Per Hour", expand = c(0, 0),
                limits=c(0, max(tweets.df$tweets))) +
            theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())
        print(pq)
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
                limits=c(0, max(tweets.df$tweets))) +
            theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.title.x = element_blank())
        print(pq)
    }

}

