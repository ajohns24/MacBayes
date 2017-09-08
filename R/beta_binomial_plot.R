#' Visualize a Beta-Binomial model
#'
#' You can use this function to visualize & summarize key features of the prior and posterior pdfs for a Beta-Binomial model.
#' @param a,b Shape parameters of the Beta prior (Beta(a,b))
#' @param x,n Observed number of successes & trials
#' @param posterior logical; if TRUE, the posterior pdf is drawn
#' @param summary logical; if TRUE, the prior/posterior mean, median, and variance are printed
#' @examples
#' library(ggplot2)
#' beta_binomial_plot(a=1, b=1)
#' beta_binomial_plot(a=1, b=1, x=5, n=10, posterior=TRUE)
#' 



beta_binomial_plot <- function(a, b, x=NULL, n=NULL, posterior=FALSE, summary=TRUE, ...){
    if(posterior==TRUE & (is.null(x) | is.null(n))) stop('Specify your data x and n')
    
    #prior pdf
    gprior <- ggplot(NULL,aes(x=c(0,1))) + 
        stat_function(fun=dbeta, args=list(shape1=a, shape2=b)) +
        stat_function(fun=dbeta, args=list(shape1=a, shape2=b), geom="ribbon", fill="cyan4", alpha=0.5, mapping = aes(ymin=0, ymax=..y..)) + 
        labs(x="p", y="pdf")
    
    if(posterior==FALSE){
        print(gprior)
        if(summary==TRUE){
            prior_mean <- a/(a+b)
            prior_mode <- (a-1)/(a+b-2)
            prior_var  <- a*b/((a+b)^2*(a+b+1))
            return(data.frame(model=c("prior"), mean=prior_mean, mode=prior_mode, var=prior_var))
        }
    }
    
    else{
        #posterior pdf
        gpost <- gprior + 
            stat_function(fun=dbeta, args=list(shape1=a+x, shape2=b+n-x)) +
            stat_function(fun=dbeta, args=list(shape1=a+x, shape2=b+n-x), geom="ribbon", fill="gold1", alpha=0.5, mapping = aes(ymin=0, ymax=..y..)) + 
            geom_vline(xintercept=x/n, color="red")
        print(gpost)
        if(summary==TRUE){
            prior_mean <- a/(a+b)
            prior_mode <- (a-1)/(a+b-2)
            prior_var  <- a*b/((a+b)^2*(a+b+1))
            post_mean  <- (x+a)/(n+a+b)    
            post_mode <- (x+a-1)/(n+a+b-2)
            post_var   <- (x+a)*(n-x+b)/((n+a+b)^2*(n+a+b+1))
            return(data.frame(model=c("prior","posterior"), mean=c(prior_mean,post_mean), mode=c(prior_mode,post_mode), var=c(prior_var,post_var)))
        }
    }
}