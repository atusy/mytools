#' kmeans+++. You can specify some centers meaningly and others randomly based on the method of kmeans++.
#' @param x input data
#' @param k number of clusters
#' @param centers_manual partial or all selection of initial centers
#' @param scale TRUE or FALSE to scale x
#' @param iter.max the maximum number of iterations allowed.
#' @param nstart if centers is a number, how many random sets should be chosen?
#' @param algorithm default to Lloyd
#'
#' @export
kmeansp3 <- function(x, k = 2, centers_manual = NULL, scale = TRUE, iter.max = 1000, nstart = 1, algorithm = "Lloyd") {
	t <- proc.time()
	x <- as.matrix(x)
	n <- nrow(x) # number of data points
	centers <- numeric(k) # IDs of centers
	distances <- matrix(numeric(n * (k - 1)), ncol = k - 1) # distances[i, j]: The distance between x[i,] and x[centers[j],]
	res_best <- list(tot.withinss = Inf) # the best result among <nstart> iterations
	pr0 <- rep(1, n)

	#define some centers manually
	j <- 0
	if(!is.null(centers_manual)) {
		if(is.vector(centers_manual)) {
			centers_manual <- x[centers_manual, ]
		} else {
			centers_manual <- as.matrix(centers_manual)
		}
		j <- nrow(centers_manual)
	}

	#scale of data and initial centers
	if(scale) {
		xmean <- colMeans(x)
		x <- t(t(x) - xmean)
		xsd <- apply(x, 2, sd)
		x <- t(t(x)/xsd)
		if(!is.null(centers_manual)) {
			centers_manual <- t((t(centers_manual) - xmean) / xsd)
		}
	}

	#kmeans
	if(j >= k){
		res_best <- kmeans(x, centers = centers_manual, iter.max = iter.max, nstart = 1, algorithm = algorithm)
		res_best$initial.centers <- centers_manual
	} else {
		if(j > 0){
			for(i in 1:j){
				distances[, i] <- colSums((t(x) - centers_manual[i, ])^2)
			}
			pr0 <- distances[cbind(1:n, max.col(-distances[, 1:j, drop = FALSE]))]
		}
		j <- j + 1

		for (rep in 1:nstart) {
			pr <- pr0 # probability for sampling centers
			if(j < k - 1){
				for (i in j:(k - 1)) {
					centers[i] <- sample.int(n, 1, prob = pr) # Pick up the ith center
					distances[, i] <- colSums((t(x) - x[centers[i], ])^2) # Compute (the square of) distances to the center
					pr <- distances[cbind(1:n, max.col(-distances[, 1:i, drop = FALSE]))] # Compute probaiblity for the next sampling
				}
			}
			centers[k] <- sample.int(n, 1, prob = pr)

			centers_auto <- rbind(centers_manual, x[centers[j:k], ])

			## Perform k-means with the obtained centers
			res <- kmeans(x, centers = centers_auto, iter.max = iter.max, nstart = 1, algorithm = algorithm)
			res$initial.centers <- centers_auto

			## Store the best result
			if (res$tot.withinss < res_best$tot.withinss) {
				res_best <- res
			}
		}
	}

	#unnormalize centers and initial.centers
	if(scale) {
		res_best$centers <- t((t(res_best$centers)*xsd)+xmean)
		res_best$initial.centers <- t((t(res_best$initial.centers)*xsd)+xmean)
	}
	res_best
}

