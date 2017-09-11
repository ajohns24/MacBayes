#' Summarize features of a Beta-Binomial model
#'
#' Summarize the parameters, mean, mode, & variance of the prior and posterior pdfs for a Beta-Binomial model.
#' @param a,b Shape parameters of the Beta prior (Beta(a,b))
#' @param x,n Observed number of successes & trials
#' @param posterior logical; if TRUE, the posterior pdf is summarized
#' @examples
#' beta_binomial_summary(a=1, b=1)
#' beta_binomial_summary(a=1, b=1, x=5, n=10, posterior=TRUE)
#' @export
beta_binomial_summary <- function(a, b, x=NULL, n=NULL, posterior=FALSE){
    if(posterior==TRUE & (is.null(x) | is.null(n))) stop('Specify your data x and n')

    if(posterior==FALSE){
        prior_mean <- a/(a+b)
        prior_mode <- (a-1)/(a+b-2)
        prior_var  <- a*b/((a+b)^2*(a+b+1))
        return(data.frame(model=c("prior"), a=a, b=b, mean=prior_mean, mode=prior_mode, var=prior_var))
    }

    else{
        prior_mean <- a/(a+b)
        prior_mode <- (a-1)/(a+b-2)
        prior_var  <- a*b/((a+b)^2*(a+b+1))
        post_mean  <- (x+a)/(n+a+b)
        post_mode <- (x+a-1)/(n+a+b-2)
        post_var   <- (x+a)*(n-x+b)/((n+a+b)^2*(n+a+b+1))
        post_a <- x+a
        post_b <- n-x+b
        return(data.frame(model=c("prior","posterior"), a=c(a,post_a), b=c(b,post_b), mean=c(prior_mean,post_mean), mode=c(prior_mode,post_mode), var=c(prior_var,post_var)))
    }
}
