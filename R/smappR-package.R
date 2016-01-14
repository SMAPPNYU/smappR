#' Tools for analysis of Twitter data
#' 
#' This package provides a series of functions that allow lab 
#' members to access the Mongo database of tweets in the lab computer
#' and easily compute summary statistics
#'
#' @seealso \code{\link{count.tweets}}, \code{\link{extract.tweets}}, 
#' \code{\link{extract.retweets}}, \code{\link{extract.hashtags}},
#' @name smappR-package
#' @aliases smappR
#' @docType package
#' @author Pablo Barbera \email{pablo.barbera@@nyu.edu}
#' @import rmongodb
#' @import ROAuth
#' @import ggplot2
#' @import httr
#' @import rjson
NULL


#' Posterior samples of ideology estimates for elites
#'
#' List that contains posterior samples of ideology and popularity estimates
#' for elites (politicians, media outlets, and journalists)
#'
#' @docType data
#' @keywords datasets
#' @name posterior_samples
#' @usage data(posterior_samples)
NULL

#' Summary ideology estimates for elites
#'
#' Data frame that contains ideal points for elites
#'
#' @docType data
#' @keywords datasets
#' @name refdata
#' @usage data(refdata)
NULL