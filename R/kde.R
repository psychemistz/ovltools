#' Calculate distribution overlap by KDE
#'
#' @param x1 A numeric vector.
#' @param x2 A numeric vector.
#' @param n An integer. Number of bins in KDE.
#' @return estimated overlap between two distributions (range: 0 to 1)
#' @examples
#' x1 = rnorm(100, 0)
#' x2 = rnorm(100, 1)
#' ovl_kde(x1, x2, n=1024)
#' @export
#' @import dplyr
#' @description
#' Calculate distribution overlap by KDE (Kernel Density Estimation)
#' This function compares two estimated distributions by KDE method.
#' It utilizes base density function in R.
#' @references
#' This code is adopted from overlapping package (\href{https://doi.org/10.3389/fpsyg.2019.01089}{paper})
#' and modified by Seongyong Park.
#' You can find original implementation in overlapping R package.
#' (\href{https://github.com/masspastore/overlapping}{github})
#' @author Seongyong Park (2020-08-18)
#' @seealso \code{\link[overlapping]{overlap}}

ovl_kde <- function(x1, x2, n = 1024) {

    get_density <- function(x, n = 1024, from = from, to = to) {
        res <- density(x, n = n, from = from, to = to)
        return(data.frame(x = res$x, y = res$y))
    }

    min_x <- min(min(x1), min(x2))
    max_x <- max(max(x1), max(x2))

    d_y1 <- get_density(x1, n = n, from = min_x, to = max_x)
    d_y2 <- get_density(x2, n = n, from = min_x, to = max_x)

    OVL <- d_y1 %>% dplyr::mutate(y1 = y) %>% dplyr::left_join(d_y2 %>% mutate(y2 = y), by = "x") %>%
        dplyr::select(x, y1, y2) %>% dplyr::group_by(x) %>%
        dplyr::summarise(OVL = min(y1, y2), OVA = max(y1, y2), .groups = "drop") %>% dplyr::ungroup() %>%
        dplyr::summarise(total = sum(OVL) / sum(OVA), .groups = "drop") %>%
        unlist(use.names = FALSE)

    return(OVL)
}
