#' Calculate distribution overlap by histogram
#'
#' @param x1 A numeric vector.
#' @param x2 A numeric vector.
#' @examples
#' x1 = rnorm(100, 0)
#' x2 = rnorm(100, 1)
#' ovl_hist(x1, x2)
#' @description
#' Calculate distribution overlap by histogram. Width of bin is adjusted according to the number of samples.
#' This function compares two estimated distributions by histogram.
#' It utilizes hist function in R.
#' @references
#' This code is adopted from GCNR paper. (\href{https://ieeexplore.ieee.org/document/8918059}{paper})
#' The original code was matlab but I implemented it in the R.
#' No code base is available. please contact to Alfonso <\email{alfonso.r.molares@@ntnu.no}>
#' The original implementation used fixed sized bin (n=100) but I used adaptive bin size
#' selection method proposed in PRE. (\href{https://journals.aps.org/pre/abstract/10.1103/PhysRevE.71.066208}{paper})
#' @author Seongyong Park (2020-08-18)

ovl_hist <- function(x1, x2) {

    x <- range(min(x1, x2), max(x1, x2))
    len_x <- min(length(x1), length(x2))
    x <- seq(x[1], x[2], length.out = 1 + log2(len_x))

    pdf_t <- hist(x1, x, plot = FALSE)
    pdf_b <- hist(x2, x, plot = FALSE)

    ovl <- sum(apply((rbind(pdf_t$counts / sum(pdf_t$counts), pdf_b$counts / sum(pdf_b$counts))), 2, min))

    return(ovl)
}
