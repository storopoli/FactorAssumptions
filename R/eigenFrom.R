#' Identify the Data Type to Obtain the Eigenvalues
#'
#' The \code{eigenFrom} function identifies the data type from which to obtain the eigenvalues. The function is used internally in many functions of the package to be able to apply these to a vector of eigenvalues, a matrix of correlations or covariance or a \code{data.frame}.
#'
#' @param x numeric: a \code{vector} of eigenvalues, a \code{matrix} of correlations or of covariances or a \code{data.frame} of data
#'
#' @return \item{value}{ character: return the data type to obtain the eigenvalues: \code{"eigenvalues"}, \code{"correlation"} or \code{"data"} }
#'
#' @examples
#' \dontshow{set.seed(123); x   <- matrix(rnorm(1000),ncol=10);}
#' \donttest{eigenFrom(x)}
#' @export

eigenFrom <-
  function(x) {
    classType <- class(x)

    res <- switch (classType,
                   data.frame  = "data",
                   matrix      = "correlation",
                   numeric     = "eigenvalues",
                   stop("Not a data.frame, a matrix, or a numeric vector")
    )

    switch (res,
            data        = if (dim(x)[2] <= 2) stop("At least 3 variables must be supplied"),
            correlation = if (dim(x)[2] <= 2) stop("At least 3 variables must be supplied"),
            eigenvalues = if (length(x) <= 2) stop("A vector of 3 eigenvalues or more must be supplied")
    )

    if (res == "correlation") if (any(x[lower.tri(x)] != t(x)[lower.tri(t(x))])) {
      stop("A correlation/covariance matrix must be symetric, empirical data must
        come from a data.frame, or eigenvalues must directly come from a vector.
        Verify the documentation about the eigenFrom function.")
    }

    invisible(res)
  }
