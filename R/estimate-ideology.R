#' @rdname estimate.ideology
#' @export
#'
#' @title 
#' Estimates ideology for a given Twitter user
#'
#' @description
#' \code{estimate.ideology} estimates ideology for a given user using the
#' Metropolis algorithm developed in Barbera, 2013. It takes as argument
#' of the function a list of user IDs indicating who a given user follows.
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#'
#' @param user screen name of user for which ideology is to be estimated.
#' 
#' @param friends vector of user IDs that the user for which ideology wants
#' to be estimated follows
#'
#' @param verbose logical, default is \code{TRUE}, which generates some output 
#' to the R console with information about progress of the sampler.
#' 
#' @param iters number of iterations of the metropolis algorithm. Default is 5000
#'
#' @param n.warmup warmup period for the sampler. Default is 1000 iterations.
#'
#' @param thin thinning of the sampler. Default is 20.
#'
#' @examples \dontrun{
#' ## download list of friends for a given user
#'  friends <- getFriends(screen_name = "p_barbera", oauth_folder="oauth")
#' ## estimating ideology 
#'  results <- estimate.ideology(friends)
#' }
#'

estimate.ideology <- function(user, friends, verbose=TRUE, iters=5000, n.warmup=1000, 
    thin=20){

    # loading posterior samples
    data(posterior_samples)
    # getting row of adjacency matrix
    y <- posterior_samples$id %in% friends
    # info message
    if (sum(y)==0){
        stop("User follows 0 elites!")
    }
    cat(user, "follows", sum(y), "elites:", 
        posterior_samples$screen_name[posterior_samples$id %in% friends], "\n")
    # estimation
    results <- metropolis.logit(y, iters=iters, n.warmup=n.warmup,
        thin=thin, verbose=TRUE)

    return(results)
}

metropolis.logit <- function(y, 
    alpha.i=posterior_samples$alpha, 
    gamma.i=posterior_samples$gamma, 
    phi.i=posterior_samples$phi, 
    beta.init=rep(log(sum(y)), chains), 
    theta.init=rnorm(chains, 0, 1), 
    mu_beta.i=posterior_samples$mu_beta, 
    sigma_beta.i=posterior_samples$sigma_beta, 
    iters=5000, delta=0.15, chains=2, n.warmup=1000, 
    thin=20, verbose=TRUE)
{
    require(R2WinBUGS, quiet=TRUE,  warn.conflicts = FALSE)
    # preparing vectors for stored samples
    keep <- seq(n.warmup+1, iters, by=thin)
    pars.samples <- array(NA, dim=c(length(keep), chains, 2),
    dimnames=list(NULL,NULL,c("beta", "theta")))
    # preparing iterations from other parameters
    options(warn=-1) # deactivating warnings for different lenghts
    alpha.it <- apply(alpha.i, 2, function(x) matrix(x, nrow=iters, ncol=1))
    phi.it <- apply(phi.i, 2, function(x) matrix(x, nrow=iters, ncol=1))
    gamma.it <- matrix(gamma.i, nrow=iters, ncol=1)
    mu_beta.it <- matrix(mu_beta.i, nrow=iters, ncol=1)
    sigma_beta.it <- matrix(sigma_beta.i, nrow=iters, ncol=1)
    options(warn=0)

    # iterations of the metropolis algorithm
    for (chain in 1:chains){
        # drawing starting points
        pars.cur <- c(beta.init[chain], theta.init[chain])
        i <- 1
        if (verbose==TRUE){ 
            message("\nChain ", chain)
            pb <- txtProgressBar(min=1,max=iters, style=3) 
        }
        # iterations
        for (iter in 1:iters){
        # getting samples from iterations
        alpha <- alpha.it[iter,]
        gamma <- gamma.it[iter]
        phi <- phi.it[iter,]
        mu_beta <- mu_beta.it[iter]
        sigma_beta <- sigma_beta.it[iter]
        # sampling candidate values
        pars.cand <- sapply(pars.cur, function(x) runif(n=1, min=x-delta, max=x+delta))
        # computing acceptance probability
        accept.prob <- exp(lpd(alpha, beta=pars.cand[1], gamma, theta=pars.cand[2], phi, mu_beta, sigma_beta, y) - 
            lpd(alpha, beta=pars.cur[1], gamma, theta=pars.cur[2], phi, mu_beta, sigma_beta, y))
        alpha <- min(accept.prob, 1)
        # jumping with probability alpha
        if (runif(1)<=alpha) { pars.cur <- pars.cand}
        # storing samples
        if (iter %in% keep) {pars.samples[i,chain,] <- pars.cur; i <- i + 1}
        if (verbose==TRUE){ setTxtProgressBar(pb, iter) }
        }
    }
      # reporting summary statistics 
  results <- round(monitor(pars.samples), 2)
  if (verbose==TRUE) {
    cat("\n")
    print(results)
    cat(chains, "chains, keeping", length(keep), 
    "iterations out of", iters, "\n")
 }
    return(list(samples=pars.samples, Rhat=results[,"Rhat"], n.eff=results[,"n.eff"]))
}

lpd <- function(alpha, beta, gamma, theta, phi, mu_beta, sigma_beta, y){
    value <- alpha + beta - gamma * (theta - phi)^2
    sum(log(plogis(value)^y * (1-plogis(value))^(1-y)))  + 
         dnorm(theta, 0, 1, log=TRUE) + dnorm(beta, mean=mu_beta, sd=sigma_beta, log=TRUE)
}

