#' Visualize a Beta-Binomial model
#'
#' Visualize & summarize key features of the prior and posterior pdfs for a Beta-Binomial model.
#' @param a,b Shape parameters of the Beta prior (Beta(a,b))
#' @param x,n Observed number of successes & trials
#' @param posterior logical; if TRUE, the posterior pdf is drawn
#' @examples
#' library(ggplot2)
#' beta_binomial_plot(a=1, b=1)
#' beta_binomial_plot(a=1, b=1, x=5, n=10, posterior=TRUE)
#' @export
beta_binomial_plot <- function(a, b, x=NULL, n=NULL, posterior=FALSE){
    if(posterior==TRUE & (is.null(x) | is.null(n))) stop('Specify your data x and n')
    
    #prior pdf
    gprior <- ggplot(NULL,aes(x=c(0,1))) + 
        stat_function(fun=dbeta, args=list(shape1=a, shape2=b)) +
        stat_function(fun=dbeta, args=list(shape1=a, shape2=b), geom="ribbon", fill="cyan4", alpha=0.5, mapping = aes(ymin=0, ymax=..y..)) + 
        labs(x="p", y="pdf")
    
    if(posterior==FALSE){
        #plot prior
        gprior
    }
    
    else{
        #posterior pdf
        gpost <- gprior + 
            stat_function(fun=dbeta, args=list(shape1=a+x, shape2=b+n-x)) +
            stat_function(fun=dbeta, args=list(shape1=a+x, shape2=b+n-x), geom="ribbon", fill="gold1", alpha=0.5, mapping = aes(ymin=0, ymax=..y..)) + 
            geom_vline(xintercept=x/n, color="red")    
        #plot posterior
        gpost
    }
}