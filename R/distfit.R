#' Calculate distribution overlap by distribution fitting
#'
#' @param x1 A numeric vector.
#' @param x2 A numeric vector.
#' @param family A string to specify the distribution family.
#' @return estimated overlap between two distributions (range: 0 to 1)
#' @examples
#' x1 = rnorm(100, 0)
#' x2 = rnorm(100, 1)
#' ovl_distfit(x1, x2, family="norm")
#' @description
#' Calculate distribution overlap by distribution fitting.
#' User need to specify which distribution family to use.
#' This function estimates population distribution from sample distributions.
#' It utilizes fitdist function in fitdistrplus R package.
#' @author Seongyong Park (2020-08-18)
#' @seealso \code{\link[fitdistrplus]{fitdist}}

ovl_distfit <- function(x1, x2, family = c("norm", "lognorm", ...)) {

    x <- range(min(x1, x2), max(x1, x2))
    len_x <- min(length(x1), length(x2))
    x <- seq(x[1], x[2], length.out = 1 + log2(len_x))

    fit_t <- fitdistrplus::fitdist(x1, family)
    fit_b <- fitdistrplus::fitdist(x2, family)

    pd_t <- pnorm(x, fit_t$estimate[1], fit_t$estimate[2])
    pd_b <- pnorm(x, fit_b$estimate[1], fit_b$estimate[2])

    OVL <- sum(apply(cbind(diff(pd_b), diff(pd_t)), 1, min))

    return(OVL)
}
