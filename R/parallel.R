#' Parallel Analysis of a Correlation or Covariance Matrix
#'
#' This function gives the distribution of the eigenvalues of correlation or a
#' covariance matrices of random uncorrelated standardized normal variables.
#' The mean and a selected quantile of this distribution are returned.
#'
#' Note that if the decision is based on a quantile value rather than on the
#' mean, care must be taken with the number of replications (\code{rep}). In
#' fact, the smaller the quantile (\code{quantile}), the bigger the number of
#' necessary replications.
#'
#' @param subject numeric: nmber of subjects (default is 100)
#' @param var numeric: number of variables (default is 10)
#' @param rep numeric: number of replications of the correlation matrix (default is 1000)
#' @param quantile numeric: quantile of the distribution on which the decision is made (default is 0.05)
#' @param model character: \code{"components"} or \code{"factors"}
#' @param sd numeric: vector of standard deviations of the simulated variables
#' (for a parallel analysis on a covariance matrix)
#' @param ...  variable: other parameters for the \code{"mvrnorm"}, \code{corr} or \code{cov} functions
#'
#' @return A list with \enumerate{
#' \item{eigen}{ Data frame consisting of the mean and the quantile of
#' the eigenvalues distribution }
#' \item{eigen$mevpea}{ Mean of the eigenvalues distribution}
#' \item{eigen$sevpea}{ Standard deviation of the eigenvalues distribution}
#' \item{eigen$qevpea}{ quantile of the eigenvalues distribution}
#' \item{eigen$sqevpea}{ Standard error of the quantile of the eigenvalues distribution}
#' \item{subject}{ Number of subjects}
#' \item{variables}{ Number of variables}
#' \item{quantile}{ Selected quantile}
#' }
#'
#' @references Drasgow, F. and Lissak, R. (1983) Modified parallel analysis: a
#' procedure for examining the latent dimensionality of dichotomously scored
#' item responses. \emph{Journal of Applied Psychology, 68}(3), 363-373.
#'
#' Hoyle, R. H. and Duvall, J. L. (2004). Determining the number of factors in
#' exploratory and confirmatory factor analysis.  In D. Kaplan (Ed.): \emph{The
#' Sage handbook of quantitative methodology for the social sciences}. Thousand
#' Oaks, CA: Sage.
#'
#' Horn, J. L. (1965). A rationale and test of the number of factors in factor
#' analysis. \emph{Psychometrika, 30}, 179-185.
#' @export

parallel <-
function(subject=100, var=10, rep=1000, quantile=0.05, model="components", sd=diag(1,var), ...)
 {
  r             <- subject
  c             <- var
  y             <- matrix(c(1:r*c), nrow=r, ncol=c)
  ycor          <- matrix(c(1:c*c), nrow=c, ncol=c)
  evpea         <- NULL
  leg.txt       <- "Pearson"

  # Simulation of k samples to obtain k random eigenvalues vectors
  # for Pearson correlation coefficients
  for (k in c(1:rep)) {
   y <- mvrnorm(n = r, mu=rep(0,var), Sigma=sd, empirical=FALSE)
   corY <- cov(y, ...)
   if (model == "components") diag(corY) <- diag(sd) # To constraint the diagonal to sd for PCA
   if (model == "factors") corY <- corY - ginv(diag(diag(ginv(corY)))) # To constraint the diagonal to communalities for FCA
   evpea          <- rbind(evpea, eigen(corY)[[1]])
   }
  # Temporay function to compute the standard error of a quantile
  SEquantile <- function(sd, n = 100, p = 0.95) {return(sd/sqrt(n) * sqrt(p*(1-p))/dnorm(qnorm(p))) }

  # Summary statistics
  sprob         <- c(quantile)
  mevpea        <- sapply(as.data.frame(evpea),  mean)                          # Eigenvalues means
  sevpea        <- sapply(as.data.frame(evpea),  sd  )                          # Eigenvalues Standard deviations
  qevpea        <- moreStats(evpea, quantile=quantile)[3,]                      # Eigenvalues quantiles
  sqevpea       <- sapply(as.data.frame(sevpea), SEquantile, n = rep, p = quantile) # Standard error of the quantile

  # List of results return
  result        <- list(eigen     = data.frame(mevpea, sevpea, qevpea, sqevpea),
                        subject   = r,
                        variables = c,
                        quantile   = quantile
                        )
  return(result)
 }
