#' Plots a Scree Plot to determine how many factors (or components to retain)
#'
#' \code{how_many_factors()} call upon the \code{\link[nFactors]{plotnScree}} function from \code{nFactors} package to iterate over the variables of a dataframe.
#'
#' Determine how many factors to retain based on:
#' \enumerate{
#' \item{Eigenvalues}
#' \item{Parallel Analysis}
#' \item{Optimal Coordinates}
#' \item{Acceleration Factor}
#' }
#'
#' @param df a dataframe with only \code{int} or \code{num} type of variables
#' @param rep numeric: number of replications of the correlation matrix (default is 1000)
#' @param cent numeric: quantile of the distribution on which the decision is made (default is 0.05)
#'
#' @import nFactors
#'
#' @examples \dontshow{set.seed(123); df <- as.data.frame(matrix(rnorm(1000),100,10));}how_many_factors(df)
#'
#' @seealso
#' \link[nFactors]{plotnScree}
#' @export

how_many_factors <- function(df, rep=1000, cent=0.05){
  # Determining the Number of Factors to Extract
  ev <- eigen(cor(df)) # get eigenvalues
  ap <- parallel(subject=nrow(df),var=ncol(df),
                 rep=rep,cent=cent)
  nS <- nScree(x = ev$values, aparallel = ap$eigen$qevpea)
  plotnScree(nS)
}
