#' kmeans++ from http://mahito.info/kmeansp2.html
#' @param x numeric matrix of data, or an object that can be coerced to such a matrix (such as a numeric vector or a data frame with all numeric columns)
#' @param k number of clusters
#' @param iter.max the maximum number of iterations allowed
#' @param nstart if centers is a number, how many random sets should be chosen?
#' @param ... additional arguments given to kmeans()
#' @export
kmeansp2 <- function(x, k, iter.max = 10, nstart = 1, ...) {
  n <- nrow(x) # number of data points
  centers <- numeric(k) # IDs of centers
  distances <- matrix(numeric(n * (k - 1)), ncol = k - 1) # distances[i, j]: The distance between x[i,] and x[centers[j],]
  res.best <- list(tot.withinss = Inf) # the best result among <nstart> iterations
  for (rep in 1:nstart) {
    pr <- rep(1, n) # probability for sampling centers
    for (i in 1:(k - 1)) {
      centers[i] <- sample.int(n, 1, prob = pr) # Pick up the ith center
      distances[, i] <- colSums((t(x) - x[centers[i], ])^2) # Compute (the square of) distances to the center
      pr <- distances[cbind(1:n, max.col(-distances[, 1:i, drop = FALSE]))] # Compute probaiblity for the next sampling
    }
    centers[k] <- sample.int(n, 1, prob = pr)
    ## Perform k-means with the obtained centers
    res <- kmeans(x, x[centers, ], iter.max = iter.max, nstart = 1, ...)
    res$inicial.centers <- x[centers, ]
    ## Store the best result
    if (res$tot.withinss < res.best$tot.withinss) {
      res.best <- res
    }
  }
  res.best
}
