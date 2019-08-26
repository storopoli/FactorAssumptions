#' Computes Eigenvalues According to the Data Type
#'
#' The \code{eigenComputes} function computes eigenvalues from the identified data type. It is used internally in many functions o in order to apply these to a vector of eigenvalues, a matrix of correlations or covariance or a data frame.
#'
#' @param x numeric: a \code{vector} of eigenvalues, a \code{matrix} of correlations or of covariances or a \code{data.frame} of data
#' @param cor logical: if \code{TRUE} computes eigenvalues from a correlation matrix, else from a covariance matrix
#' @param model character: \code{"components"} or \code{"factors"}
#' @param ...  variable: additionnal parameters to give to the \code{cor} or \code{cov} functions
#'
#' @return \item{value}{ numeric: return a vector of eigenvalues }
#'
#' @export

eigenComputes <-
  function(x, cor=TRUE, model="components", ...) {
    dataType <- eigenFrom(x)

    if (model == "components") {
      res <- switch(dataType,
                    eigenvalues = as.vector(x),
                    correlation = {if (cor == FALSE) eigen(x)$values           else  eigen(cov2cor(x))$values},
                    data        = {if (cor == TRUE)  eigen(cor(x, ...))$values else  eigen(cov(x, ...))$values}
      )
    }

    if (model == "factors") {
      res <- switch(dataType,
                    eigenvalues = as.vector(x),
                    correlation = {if (cor == FALSE) eigen(corFA(x, method="ginv"))$values else   eigen(cov2cor(corFA(x, method="ginv")))$values},
                    data        = {if (cor == TRUE)  eigen(corFA(cor(x, ...),method="ginv"))$values else  eigen(corFA(cov(x, ...),method="ginv"))$values}
      )
    }
    return(res)
  }
