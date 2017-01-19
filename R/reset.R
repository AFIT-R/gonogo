#' Quick reset of graphical parameters
#'
#' \code{reset} resets the graphical parameters (\code{par}) arguments. Primarily
#' leveraged by \code{gonogo} plotting functions where parameters are adjusted
#' but are then reset after the plotting outputs are produced.
#'
#' @export

reset <- function(){
  par(
    mar = c(5, 4, 4, 2) + .1,
    oma = c(0, 0, 0, 0),
    mgp = c(3, 1, 0)
  )

  return()
}
