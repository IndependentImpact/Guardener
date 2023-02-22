
#' assignNames
#'
#' @param x list
#' @author Alex Howard
#' @return
#' @examples
#' @export
#'
assignNames <- function(x) {

  if ("children" %in% names(x)) {
    if (length(x$children) > 0) {
      names(x$children) <- sapply(X = x$children, FUN = function(y) y$tag)
      x$children <- lapply(X = x$children, FUN = assignNames)
    }
  }

  return(x)
}
