#' Visualize a Poisson-Gamma model
#'
#' Visualize the prior and posterior pdfs for a Poisson-Gamma model.
#' @param s,r Shape & rate parameters of the Gamma prior (Gamma(s,r))
#' @param sum_x,n Observed count total & sample size, respectively
#' @examples
#' library(ggplot2)
#' poisson_gamma_plot(s=2, r=2)
#' poisson_gamma_plot(s=2, r=2, sum_x=10, n=5)
#' @export
poisson_gamma_plot <- function(s, r, sum_x=NULL, n=NULL){
    if(is.null(sum_x) | is.null(n)) warning('To visualize the posterior, specify data sum_x and n')
    xmin <- min(qgamma(0.00001,s,r), qgamma(0.00001,s+sum_x,r+n))
    xmax <- max(qgamma(0.99999,s,r), qgamma(0.99999,s+sum_x,r+n))
    #prior pdf
    gprior <- ggplot(NULL,aes(x=c(xmin,xmax))) +
        stat_function(fun=dgamma, args=list(shape=s, rate=r)) +
        stat_function(fun=dgamma, args=list(shape=s, rate=r), geom="ribbon", fill="gold1", alpha=0.5, mapping = aes(ymin=0, ymax=..y..)) +
        labs(x=expression(lambda), y="pdf")

    if(is.null(sum_x) & is.null(n)){
        #plot prior
        gprior
    }

    else{
        #posterior pdf
        gpost <- gprior +
            stat_function(fun=dgamma, args=list(shape=s+sum_x, rate=r+n)) +
            stat_function(fun=dgamma, args=list(shape=s+sum_x, rate=r+n), geom="ribbon", fill="cyan4", alpha=0.5, mapping = aes(ymin=0, ymax=..y..)) +
            geom_vline(xintercept=sum_x/n, color="red")
        #plot posterior
        gpost
    }
}
