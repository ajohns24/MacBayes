#' Simulated NBA box scores
#'
#' Simulated data on 50 NBA home games for Team A.  Scores for Team A
#' and their opponent, as well as the differences between them, are
#' recorded after each of 4 quarters.
#'
#' @docType data
#'
#' @usage data(boxscore)
#'
#' @format A data.frame with 50 observations on 12 variables.
#'   \itemize{
#'     \item{AQ1}{Home Team A score at the end of the 1st quarter [numeric]}
#'     \item{AQ2}{Home Team A score at the end of the 2nd quarter [numeric]}
#'     \item{AQ3}{Home Team A score at the end of the 3rd quarter [numeric]}
#'     \item{AQ4}{Home Team A score at the end of the 4th quarter [numeric]}
#'     \item{BQ1}{Opponent score at the end of the 1st quarter [numeric]}
#'     \item{BQ2}{Opponent score at the end of the 2nd quarter [numeric]}
#'     \item{BQ3}{Opponent score at the end of the 3rd quarter [numeric]}
#'     \item{BQ4}{Opponent score at the end of the 4th quarter [numeric]}
#'     \item{Diff1}{Score differential at the end of the 1st quarter [numeric]}
#'     \item{Diff2}{Score differential at the end of the 2nd quarter [numeric]}
#'     \item{Diff3}{Score differential at the end of the 3rd quarter [numeric]}
#'     \item{Diff4}{Score differential at the end of the 4th quarter [numeric]}
#'   }
#'
#' @source simulation
"boxscore"
