#' Statistical Summary of a Data Frame
#'
#' This function produces another summary of a \code{data.frame}. This function was proposed in order to apply some functions globally on a \code{data.frame}: \code{quantile}, \code{median}, \code{min} and \code{max}. The usual \emph{R} version cannot do so.
#'
#' @param x numeric: matrix or \code{data.frame}.
#' @param quantile numeric: quantile of the distribution.
#' @param show logical: if \code{TRUE} prints the quantile choosen.
#'
#' @return \item{values}{ numeric: \code{data.frame} of statistics: mean, median, quantile, standard deviation, minimum and maximum. }
#' @seealso \code{\link{nScree}}
#'
#' @export

moreStats <-
  function(x, quantile=0.95, show=FALSE) {
    x     <- data.frame(x)
    xMean <- sapply(x, mean) # mean(x)
    xSd   <- sapply(x, sd)   # sd(x)
    xMin  <- xMax <- xMedian <- xQuantile <- numeric(ncol(x))
    for (i in 1:ncol(x)) {
      xMin[i]    <- min(x[,i])
      xMax[i]    <- max(x[,i])
      xMedian[i] <- median(x[,i])
      xQuantile[i]  <- quantile(x[,i],probs=quantile,names=FALSE, na.rm=TRUE)
    }
    names       <- colnames(x)
    results     <- rbind(mean=xMean, median=xMedian, quantile=xQuantile, sd=xSd, min=xMin, max=xMax)
    if (show==TRUE) {
      cat("------------------------ \n")
      cat("Quantile specified:", quantile, "\n")
      cat("------------------------ \n")
    }
    return(results)
  }
