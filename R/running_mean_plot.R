#' Plot a running mean
#'
#' Visualize the running, i.e. cumulative, mean of a vector.
#' @param x a vector
#' @param se logical; if TRUE, naive confidence bands are included
#' @param fill fill color of confidence bands
#' @param alpha transparency of confidence bands
#' @examples
#' library(ggplot2)
#' x <- rnorm(1000)
#' running_mean_plot(x)
#' running_mean_plot(x, se=TRUE)
#' @export
running_mean_plot <- function(x, se=FALSE, fill="gray", alpha=0.9){
    N <- length(x)
    #Calculate the length and running mean of vector x
    run_data <- data.frame(n=1:N, run_mean=cumsum(x)/c(1:N))

    if(se==FALSE){
        g1 <- ggplot(run_data, aes(x=n, y=run_mean)) +
            geom_line() +
            labs(y="running mean")
        g1
    }

    else{
        #Calculate the running (naive) margin of error
        moe <- rep(0,N-1)
        for(i in 2:(N-1)){
            moe[i-1] <- 1.96*sd(x[1:i])/sqrt(i)
        }
        run_data$moe <- c(NA, moe)


        #Plot the running mean versus sample size:
        g2 <- ggplot(run_data, aes(x=n, y=run_mean)) +
            geom_ribbon(aes(ymin=run_mean-moe, ymax=run_mean+moe), fill=fill, alpha=alpha) +
            geom_line() +
            labs(y="running mean")
        g2
    }
}
