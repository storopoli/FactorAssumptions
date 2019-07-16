#' Calculates the Optimal Solution for Communalities in a Dataframe
#'
#' \code{communalities_optimal_solution()} call upon the \code{\link[psych]{principal}} function from \code{psych} package to iterate over the variables of a dataframe.
#'
#'If finds any individual communality below the optimal value of 0.5 then removes the lowest communality value variable until no more variable has not-optimal communality values.
#'
#' @param df a dataframe with only \code{int} or \code{num} type of variables
#' @param nfactors number of factors to extract in principal components or factor analysis
#' @param rotate rotation to be employed (default is varimax)
#'
#' @return A list with \enumerate{
#' \item \code{df} - A dataframe that has reached its optimal solution in terms of KMO values
#' \item \code{removed} - A list of removed variables ordered by the first to last removed during the procedure
#' \item \code{loadings} - A table with the communalities loadings from the variables final iteration
#' \item \code{pca_results} - Results of the final iteration of the \code{\link{principal}} function from \code{psych} package
#'}
#'
#' @import psych
#'
#' @importFrom graphics plot
#'
#' @seealso
#' \code{\link[psych]{principal}}
#' @export

communalities_optimal_solution <- function(df, nfactors, rotate="varimax"){
  removed <- c()
  results <- principal(df, nfactors = nfactors, rotate = rotate, scores = T)
  while (any(as.data.frame(as.data.frame(results$communality)) < 0.5)){
    message(sprintf("There is still an individual communality value below 0.5: "),
            rownames(as.data.frame(results$communality))[which.min(apply(as.data.frame(results$communality),MARGIN=1,min))]," - ",
            min(as.data.frame(results$communality)))
    column <- sprintf(rownames(as.data.frame(results$communality))[which.min(apply(as.data.frame(results$communality),MARGIN=1,min))])
    removed <- c(removed, column)
    df <- df[, !(colnames(df) %in% column), drop=FALSE]
    results <- principal(df, nfactors = nfactors, scores = T)
  }

  loadings <- as.table(printLoadings(results$loadings))

  fa.diagram(results, digits = 3, cut = 0.4, sort = T) # Diagram of Factors, items and loadings

  plot(results)

  return(list(
    df = df,
    removed = removed,
    loadings = loadings,
    pca_results = results))
}
