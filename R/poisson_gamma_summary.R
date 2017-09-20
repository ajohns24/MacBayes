#' Summarize features of a Poisson-Gamma model
#'
#' Summarize the parameters, mean, mode, & variance of the prior and posterior pdfs for a Poisson-Gamma model.
#' @param s,r Shape & rate parameters of the Gamma prior (Gamma(s,r))
#' @param sum_x,n Observed count total & sample size, respectively
#' @examples
#' poisson_gamma_summary(s=2, r=2)
#' poisson_gamma_summary(s=2, r=2, sum_x=10, n=5)
#' @export
poisson_gamma_summary <- function(s, r, sum_x=NULL, n=NULL){
    if(is.null(sum_x) | is.null(n)) warning('To summarize the posterior, specify data sum_x and n')

    if(is.null(sum_x) & is.null(n)){
        prior_mean <- s/r
        if(s >= 1){prior_mode <- (s-1)/r}
        else{prior_mode <- NA}
        prior_var  <- s/r^2
        return(data.frame(model=c("prior"), s=s, r=r, mean=prior_mean, mode=prior_mode, var=prior_var))
    }

    else{
        prior_mean <- s/r
        if(s >= 1){prior_mode <- (s-1)/r}
        else{prior_mode <- NULL}
        prior_var  <- s/r^2
        post_mean  <- (s+sum_x)/(r+n)
        if((s+sum_x) >= 1){post_mode <- (s+sum_x-1)/(r+n)}
        else{post_mode <- NA}
        post_var   <- (s+sum_x)/((r+n)^2)
        post_s <- s+sum_x
        post_r <- r+n
        return(data.frame(model=c("prior","posterior"), s=c(s,post_s), r=c(r,post_r), mean=c(prior_mean,post_mean), mode=c(prior_mode,post_mode), var=c(prior_var,post_var)))
    }
}
