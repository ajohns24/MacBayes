#' Visualize a Beta-Binomial model
#'
#' Visualize the prior and posterior pdfs for a Beta-Binomial model.
#' @param a,b Shape parameters of the Beta prior (Beta(a,b))
#' @param x,n Observed number of successes & trials, respectively
#' @examples
#' library(ggplot2)
#' beta_binomial_plot(a=2, b=1)
#' beta_binomial_plot(a=2, b=1, x=5, n=10)
#' @export
beta_binomial_plot <- function(alpha, beta, x = NULL, n = NULL, prior = TRUE, likelihood = TRUE, posterior = TRUE){
    if(is.null(x) | is.null(n)) warning('To visualize the posterior, specify data x and n')

    #prior pdf
    g <- ggplot(NULL, aes(x = c(0,1))) +
        labs(x = expression(theta), y = "density") +
        scale_fill_manual("", values = c("prior" = "gold1", "(scaled) likelihood" = "cyan2", "posterior" = "cyan4"))

    if(prior == TRUE){
        g <- g +
            stat_function(fun = dbeta, args=list(shape1 = alpha, shape2 = beta)) +
            stat_function(fun = dbeta, args=list(shape1 = alpha, shape2 = beta),
                          geom="area", alpha=0.5, aes(fill = "prior"))

    }

    if(!is.null(x) & !is.null(n)){
        # Posterior shape parameters
        alpha_post <- alpha + x
        beta_post  <- beta + n - x

        # Scaled likelihood function
        x_data      <- x
        like_scaled <- function(x){
            like_fun    <- function(x){dbinom(x = x_data, size = n, prob = x)}
            scale_c     <- integrate(like_fun, lower = 0, upper = 1)[[1]]
            like_fun(x) / scale_c
        }
    }

    if(!is.null(x) & !is.null(n) & (likelihood != FALSE)){
        g <- g +
            stat_function(fun = like_scaled) +
            stat_function(fun = like_scaled, geom = "area", alpha = 0.5, aes(fill = "(scaled) likelihood"))
    }

    if(!is.null(x) & !is.null(n) & posterior == TRUE){
        g <- g +
            stat_function(fun = dbeta, args = list(shape1 = alpha_post, shape2 = beta_post)) +
            stat_function(fun = dbeta, args = list(shape1 = alpha_post, shape2 = beta_post),
                          geom = "area", alpha = 0.5, aes(fill = "posterior"))
    }

    # Plot
    g
}
