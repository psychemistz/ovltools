#' Calculate distribution overlap by KNN
#'
#' @param x1 A numeric vector.
#' @param x2 A numeric vector.
#' @param k An integer. Number of k in KNN
#' @return estimated overlap between two distributions (range: 0 to 1)
#' @examples
#' x1 = rnorm(100, 0)
#' x2 = rnorm(100, 1)
#' ovl_knn(x1, x2, k=5)
#' @description
#' Calculate distribution overlap by KNN (K-Nearest neighbor) This function compares label of K-nearest neighbors and
#' calculate how many labels are mismatch.
#' @references
#' This code is adopted from AmRMR
#' (\href{https://www.mdpi.com/2073-431X/8/2/42}{paper}) and modified by
#' Seongyong Park. You can find their implementation in their websites.
#' (\href{https://bitldku.github.io/home/sw/R-value.html}{Java},
#' \href{https://bitldku.github.io/home/sw/AmRMR.html}{R}) Many thanks to the
#' professor Sejong Oh (Dankook University, \email{sejongoh@@dankook.ac.kr}) who kindly share his work.
#' @author Seongyong Park (2020-08-18)

ovl_knn <- function(x1, x2, k = 5) {

    rvalue <- 0
    ds <- cbind(c(x1, x2))  ## data matrix
    cl <- cbind(c(rep(0, length(x1)), rep(1, length(x2))))  ## class vector (column vector)



    noRow <- nrow(ds)  # no of Row
    knns <- FNN::get.knn(ds, k)  # find k nearest neighbors for all instances (FNN package)
    knns.ndx <- data.frame(knns[["nn.index"]])

    diffNeighbor <- 0
    for (i in 1:noRow) {
        thisClass <- cl[i]
        for (j in 1:k) {
            if (cl[knns.ndx[i, j]] != thisClass) {
                diffNeighbor <- diffNeighbor + 1
            }
        }
    }

    rvalue <- diffNeighbor / (noRow * k)
    return(rvalue)
}

#' Generalized version of ovl_knn (Rvalue).
#'
#' @param ds A numeric vector.
#' @param cl An integer vector designating classes.
#' @param k An integer. Number of k in KNN
#' @examples
#' ds = c(rnorm(100, 0), rnorm(100, 1), rnorm(100,2))
#' cl = c(rep(0,100), rep(1,100), rep(2,100))
#' Rvalue(ds, cl, k=5)
#' @description
#' Calculate distribution overlap by KNN (K-Nearest neighbor)
#' This function is the original implementation of Rvalue. It works with more than 3 classes.
#' The behavior of function is exactly the same as ovl_knn.
#' @references
#' This code is adopted from AmRMR.
#' (\href{https://www.mdpi.com/2073-431X/8/2/42}{paper}) You can find their implementation in their websites.
#' (\href{https://bitldku.github.io/home/sw/R-value.html}{Java},
#' \href{https://bitldku.github.io/home/sw/AmRMR.html}{R}) Many thanks to the
#' professor Sejong Oh (Dankook University, \email{sejongoh@@dankook.ac.kr}) who kindly share his work.
#' @author Seongyong Park (2020-08-18)
Rvalue <- function(ds, cl, k = 5) {
    rvalue <- 0
    if (is.vector(ds)) {
        ds <- cbind(ds)
    }
    if (is.vector(cl)) {
        cl <- cbind(cl)
    }

    noRow <- nrow(ds)  # no of Row

    knns <- FNN::get.knn(ds, k)  # find k nearest neighbors for all instances
    knns.ndx <- data.frame(knns[["nn.index"]])

    diffNeighbor <- 0
    for (i in 1:noRow) {
        thisClass <- cl[i]
        for (j in 1:k) {
            if (cl[knns.ndx[i, j]] != thisClass) {
                diffNeighbor <- diffNeighbor + 1
            }
        }
    }

    rvalue <- diffNeighbor / (noRow * k)
    return(rvalue)
}




