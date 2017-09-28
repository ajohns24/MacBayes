#' Matrix power calculation
#'
#' Calculate the nth power of a square matrix.  When the given matrix is a Markov chain transition matrix, this is equivalent to the n-step transition matrix.
#' @param mat a square matrix
#' @param n the power
#' @examples
#' x <- matrix(c(1:9), ncol=3)
#' matrix_power(mat=x, n=1)
#' matrix_power(mat=x, n=2)
#' @export
matrix_power <- function(mat, n){
    times <- 1
    mats <- mat
    repeat {
        if (times == n){break}
        mats <- mats %*% mats
        times <- times+1
    }
    return(mats)
}
