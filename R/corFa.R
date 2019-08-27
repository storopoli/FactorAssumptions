#' Insert Communalities in the Diagonal of a Correlation or a Covariance Matrix
#'
#' This function inserts communalities in the diagonal of a correlation/covariance matrix.
#'
#'
#' @param R numeric: correlation matrix.
#' @param method character: actually only \code{"ginv"} is supplied to compute the approximation of the communalities by maximum correlation.
#' @return
#' \item{values}{ numeric: matrix of correlation/covariance with communalities in the diagonal. }
#'
#' @seealso \code{\link{nScree}}
#'
#' @examples
#' \dontshow{set.seed(123); x   <- matrix(rnorm(1000),ncol=10);}
#' \donttest{corFA(x, method = "ginv")}
#'
#' @export

corFA <-
  function(R, method="ginv") {
    R <- as.matrix(R)
    if (method == "ginv") return(R - ginv(diag(diag(ginv(R)))))
  }
