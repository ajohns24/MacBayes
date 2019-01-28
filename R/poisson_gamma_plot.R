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
poisson_gamma_plot <- function(s, r, sum_x = NULL, n = NULL, prior = TRUE, likelihood = TRUE, posterior = TRUE){
    if(is.null(sum_x) | is.null(n)) warning('To visualize the posterior, specify information about the data: sum_x and n')

    x_min <- min(qgamma(0.00001,s,r), qgamma(0.00001,s+sum_x,r+n))
    x_max <- max(qgamma(0.99999,s,r), qgamma(0.99999,s+sum_x,r+n))

    #prior pdf
    g <- ggplot(NULL, aes(x = c(x_min, x_max))) +
        labs(x = expression(lambda), y = "density") +
        scale_fill_manual("", values = c("prior" = "gold1", "(scaled) likelihood" = "cyan2", "posterior" = "cyan4"))

    if(prior == TRUE){
        g <- g +
            stat_function(fun=dgamma, args=list(shape=s, rate=r)) +
            stat_function(fun=dgamma, args=list(shape=s, rate=r),
                          geom="area", alpha=0.5, aes(fill = "prior"))
    }

    if(!is.null(sum_x) & !is.null(n)){
        # Posterior shape parameters
        shape_post <- s + sum_x
        rate_post  <- r + n

        # Scaled likelihood function
        like_scaled <- function(x){
            dgamma(x, shape = sum_x + 1, rate = n)
        }
    }

    if(!is.null(sum_x) & !is.null(n) & (likelihood != FALSE)){
        g <- g +
            stat_function(fun = like_scaled) +
            stat_function(fun = like_scaled, geom = "area", alpha = 0.5, aes(fill = "(scaled) likelihood"))
    }

    if(!is.null(sum_x) & !is.null(n) & posterior == TRUE){
        g <- g +
            stat_function(fun = dgamma, args = list(shape = shape_post, rate = rate_post)) +
            stat_function(fun = dgamma, args = list(shape = shape_post, rate = rate_post),
                          geom = "area", alpha = 0.5, aes(fill = "posterior"))
    }

    # Plot
    g
}
