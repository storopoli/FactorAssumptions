#' Calculates the Optimal Solution for Communalities in a Dataframe
#'
#' \code{communalities_optimal_solution()} call upon either the \code{\link[psych]{principal}} or \code{\link[psych]{fa}} functions from \code{psych} package to iterate over the variables of a dataframe.
#'
#'If finds any individual communality below the optimal value of 0.5 then removes the lowest communality value variable until no more variable has not-optimal communality values.
#'
#' @param df a dataframe with only \code{int} or \code{num} type of variables
#' @param type either \code{principal} for \emph{Principal Components Analysis} or \code{fa} for \emph{Factor Analysis}
#' @param nfactors number of factors to extract in principal components or factor analysis
#' @param rotate rotation to be employed (default is \emph{varimax}). "none", "varimax", "quartimax", "bentlerT", "equamax", "varimin", "geominT" and "bifactor" are orthogonal rotations. "Promax", "promax", "oblimin", "simplimax", "bentlerQ, "geominQ" and "biquartimin" and "cluster" are possible oblique transformations of the solution. The default is to do a oblimin transformation, although versions prior to 2009 defaulted to varimax. SPSS seems to do a Kaiser normalization before doing Promax, this is done here by the call to "promax" which does the normalization before calling Promax in GPArotation.
#' @param fm Factoring method fm="minres" (default) will do a minimum residual as will fm="uls". Both of these use a first derivative. fm="ols" differs very slightly from "minres" in that it minimizes the entire residual matrix using an OLS procedure but uses the empirical first derivative. This will be slower. fm="wls" will do a weighted least squares (WLS) solution, fm="gls" does a generalized weighted least squares (GLS), fm="pa" will do the principal factor solution, fm="ml" will do a maximum likelihood factor analysis. fm="minchi" will minimize the sample size weighted chi square when treating pairwise correlations with different number of subjects per pair. fm ="minrank" will do a minimum rank factor analysis. "old.min" will do minimal residual the way it was done prior to April, 2017 (see discussion below). fm="alpha" will do alpha factor analysis as described in Kaiser and Coffey (1965)
#' @param squared TRUE if matrix is squared (such as adjacency matrices), FALSE otherwise
#'
#' @return A list with \enumerate{
#' \item \code{df} - A dataframe that has reached its optimal solution in terms of KMO values
#' \item \code{removed} - A list of removed variables ordered by the first to last removed during the procedure
#' \item \code{loadings} - A table with the communalities loadings from the variables final iteration
#' \item \code{results} - Results of the final iteration of either the \code{\link[psych]{principal}} or \code{\link[psych]{fa}} functions from \code{psych} package
#'}
#'
#' @importFrom psych principal
#' @importFrom psych fa
#'
#' @examples
#' set.seed(123)
#' df <- as.data.frame(matrix(rnorm(100*10, 1, .5), ncol=10))
#' communalities_optimal_solution(df, nfactors = 2,type = "principal", squared = FALSE)
#'
#' @seealso
#' \code{\link[psych]{principal}} the PCA function from psych and
#' \code{\link[psych]{fa}} the Factor Analysis function from psych
#' @export

communalities_optimal_solution <- function(df, nfactors, type, rotate="varimax", fm="minres", squared=TRUE){
  removed <- c()
  if (type == "principal"){
    results <- principal(df, nfactors = nfactors, rotate = rotate, scores = T)
    while (any(as.data.frame(as.data.frame(results$communality)) < 0.5)){
      message(sprintf("There is still an individual communality value below 0.5: "),
            rownames(as.data.frame(results$communality))[which.min(apply(as.data.frame(results$communality),MARGIN=1,min))]," - ",
            min(as.data.frame(results$communality)))
      column <- sprintf(rownames(as.data.frame(results$communality))[which.min(apply(as.data.frame(results$communality),MARGIN=1,min))])
      removed <- c(removed, column)
      if (squared == TRUE) {
        rownames(df) <- colnames(df)
        df <- df[!(rownames(df) %in% column), !(colnames(df) %in% column), drop=FALSE]
      }
      else {
        df <- df[, !(colnames(df) %in% column), drop=FALSE]
      }
      results <- principal(df, nfactors = nfactors, scores = T)
      }
  }
  else if (type == "fa"){
    results <- fa(df, nfactors = nfactors, fm = fm, rotate = rotate, scores = T)
    while (any(as.data.frame(as.data.frame(results$communality)) < 0.5)){
      message(sprintf("There is still an individual communality value below 0.5: "),
              rownames(as.data.frame(results$communality))[which.min(apply(as.data.frame(results$communality),MARGIN=1,min))]," - ",
              min(as.data.frame(results$communality)))
      column <- sprintf(rownames(as.data.frame(results$communality))[which.min(apply(as.data.frame(results$communality),MARGIN=1,min))])
      removed <- c(removed, column)
      if (squared == TRUE) {
        rownames(df) <- colnames(df)
        df <- df[!(rownames(df) %in% column), !(colnames(df) %in% column), drop=FALSE]
        rownames(df) <- colnames(df)
      }
      else {
        df <- df[, !(colnames(df) %in% column), drop=FALSE]
      }
      results <- fa(df, fm = fm, nfactors = nfactors, scores = T)
    }
  }
  else {
    message("Please choose type either principal or fa")
    stop("")
    }
  loadings <- as.table(printLoadings(results$loadings))

  return(list(
    df = df,
    removed = removed,
    loadings = loadings,
    results = results))
}
