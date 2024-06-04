#' Unscale a scaled vector
#'
#' This function unscales a scaled vector using its mean and standard deviation.
#' It throws an error if the necessary attributes (`scaled:center` and `scaled:scale`) are not present.
#'
#' @param scaled A numeric vector that has been scaled.
#' @return A numeric vector unscaled to its original values.
#' @throws Error if the `scaled:center` or `scaled:scale` attributes are not present.
#' @examples
#' scaled_vector <- scale(c(1, 2, 3, 4, 5))
#' unscaled_vector <- unscale(scaled_vector)
#' print(unscaled_vector)
unscale <- function(scaled) {
  if (is.null(attr(scaled, "scaled:center"))) {
    stop("The 'scaled:center' attribute is missing. Ensure the input vector is properly scaled.")
  }
  
  if (is.null(attr(scaled, "scaled:scale"))) {
    stop("The 'scaled:scale' attribute is missing. Ensure the input vector is properly scaled.")
  }
  
  center <- attr(scaled, "scaled:center")
  scale <- attr(scaled, "scaled:scale")
  
  original <- (scaled * scale) + center
  return(original)
}