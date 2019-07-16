#' Calculates the Optimal Solution for Kayser-Meyer-Olkin (KMO) in a Dataframe
#'
#' \code{kmo_optimal_solution()} call upon the \code{\link[FactorAssumptions]{kmo}} function to iterate over the variables of a dataframe.
#'
#'If finds any individual KMO's below the optimal value of 0.5 then removes the lowest KMO value variable untill no more variable has not-optimal KMO values.
#'
#' @param df a dataframe with only \code{int} or \code{num} type of variables
#' @return \enumerate{
#' \item \code{optimalkmodf} - A \emph{global environment} dataframe with the optimal solution output
#' \item A list with \enumerate{
#' \item \code{df} - A dataframe that has reached its optimal solution in terms of KMO values
#' \item \code{removed} - A list of removed variables ordened by the first to last removed during the procedure
#' \item \code{kmo_results} - Results of the final iteration of the \code{\link{kmo}} function
#'}
#'}
#'
#'@seealso
#' \code{\link{kmo}} for kmo computation function
#' @export

kmo_optimal_solution <- function(df){
  removed <- c()
  results <- kmo(df)
  while (any(results$individual < 0.5)){
    column <- sprintf(rownames(results$individual)[which.min(apply(results$individual,MARGIN=1,min))])
    #row <- match(column,names(df))
    #row <- as.numeric(gsub("([0-9]+).*$", "\\1", row))
    removed <- c(removed, column)
    df <- df[, !(colnames(df) %in% column), drop=FALSE]
    #df <- df[-row, ]
    #rownames(df) <- NULL
    results <- kmo(df)
  }

  #write.csv(results$individual, "kmo_individual.csv")
  assign("optimalkmodf", df, envir=globalenv())
  return(list(
    df = df,
    removed = removed,
    results = results))

}
