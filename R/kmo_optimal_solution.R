#' Calculates the Optimal Solution for Kayser-Meyer-Olkin (KMO) in a Dataframe
#'
#' \code{kmo_optimal_solution()} call upon the \code{\link[FactorAssumptions]{kmo}} function to iterate over the variables of a dataframe.
#'
#'If finds any individual KMO's below the optimal value of 0.5 then removes the lowest KMO value variable until no more variable has not-optimal KMO values.
#'
#' @param df a dataframe with only \code{int} or \code{num} type of variables
#' @param squared TRUE if matrix is squared (such as adjacency matrices), FALSE otherwise
#' @return A list with \enumerate{
#' \item \code{df} - A dataframe that has reached its optimal solution in terms of KMO values
#' \item \code{removed} - A list of removed variables ordened by the first to last removed during the procedure
#' \item \code{kmo_results} - Results of the final iteration of the \code{\link{kmo}} function
#'}
#' @examples
#' set.seed(123)
#' df <- as.data.frame(matrix(rnorm(100*10, 1, .5), ncol=10))
#' kmo_optimal_solution(df, squared = FALSE)
#'
#'@seealso
#' \code{\link{kmo}} for kmo computation function
#' @export

kmo_optimal_solution <- function(df, squared=TRUE){
  removed <- c()
  df <- as.data.frame(df)
  results <- kmo(df, squared=squared)
  while (any(results$individual < 0.5)){
    column <- sprintf(rownames(results$individual)[which.min(apply(results$individual,MARGIN=1,min))])
    removed <- c(removed, column)
    if (squared == TRUE) {
      rownames(df) <- colnames(df)
      df <- df[!(rownames(df) %in% column), !(colnames(df) %in% column), drop=FALSE]
      rownames(df) <- colnames(df)
    }
    else {
      df <- df[, !(colnames(df) %in% column), drop=FALSE]
    }
    results <- kmo(df, squared = squared)
  }
  return(list(
    df = df,
    removed = removed,
    results = results))
}

