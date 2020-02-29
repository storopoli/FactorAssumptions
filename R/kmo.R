#' Calculates the Kayser-Meyer-Olkin (KMO)
#'
#' \code{kmo()} handles both positive definite and not-positive definite matrix by employing the \emph{Moore-Penrose} inverse (pseudoinverse)
#'
#' @param x a matrix or dataframe
#' @param squared TRUE if matrix is squared (such as adjacency matrices), FALSE otherwise
#' @return A list with \enumerate{
#' \item \code{overall} - Overall KMO value
#' \item \code{individual} - Individual KMO's dataframe
#' \item \code{AIS} - Anti-image Covariance Matrix
#' \item \code{AIR} - Anti-image Correlation Matrix
#'}
#'
#' @importFrom MASS ginv
#' @importFrom stats cor
#' @importFrom stats sd
#'
#' @examples
#' set.seed(123)
#' df <- as.data.frame(matrix(rnorm(100*10, 1, .5), ncol=10))
#' kmo(df, squared = FALSE)
#' @export

kmo = function(x, squared=TRUE){
  if (squared == TRUE) {
    stopifnot(nrow (x) == ncol(x))
    rownames(x) <- colnames(x)
    # checking for sd = 0 and removing row and col
    x <- x[!sapply(x, function(x) { sd(x) == 0} ), !sapply(x, function(x) { sd(x) == 0} )]
    X <- cor(as.matrix(x))
  }
  else {
    X <- cor(as.matrix(x))
  }
  iX <- ginv(X)
  S2 <- diag(diag((iX^-1)))
  AIS <- S2%*%iX%*%S2                      # anti-image covariance matrix
  IS <- X+AIS-2*S2                         # image covariance matrix
  Dai <- sqrt(diag(diag(AIS)))
  IR <- ginv(Dai)%*%IS%*%ginv(Dai)         # image correlation matrix
  AIR <- ginv(Dai)%*%AIS%*%ginv(Dai)       # anti-image correlation matrix
  a <- apply((AIR - diag(diag(AIR)))^2, 2, sum)
  AA <- sum(a)
  b <- apply((X - diag(nrow(X)))^2, 2, sum)
  BB <- sum(b)
  MSA <- b/(b+a)                        # indiv. measures of sampling adequacy

  AIR <- AIR-diag(nrow(AIR))+diag(MSA)  # Examine the anti-image of the correlation matrix. That is the negative of the partial correlations, partialling out all other variables.

  kmo_overall <- BB/(AA+BB)                     # overall KMO statistic

  individual = as.data.frame(MSA)

  ans <- list(  overall = kmo_overall,
                individual = individual,
                AIS = AIS,
                AIR = AIR)
  if (any(individual < 0.5)){
    message(sprintf("There is still an individual KMO value below 0.5: "),
            rownames(individual)[which.min(apply(individual,MARGIN=1,min))]," - ",
            min(individual))
  } else {
    message("Final Solution Achieved!")
  }
  return(ans)

}    # end of kmo()
