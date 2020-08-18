## This code is adopted from overlapping paper.  Reference: Pastore, M.,Calcagnì, A. (2019). Measuring Distribution Similarities Between
## Samples: A Distribution-Free Overlapping Index. Frontiers in Psychology, 10:1089. URL: https://doi.org/10.3389/fpsyg.2019.01089
## Codebase: https://github.com/masspastore/overlapping

## Modified by Seongyong Park (2020-08-18) overlap function in overlapping package provides distribution overlap based on kernel density
## estimation (KDE).  The original package used base density function to perform KDE.

ovl_kde <- function(x1, x2, n = 1024) {

    get_density <- function(x, n = 1024, from = from, to = to) {
        res <- density(x, n = n, from = from, to = to)
        return(data.frame(x = res$x, y = res$y))
    }

    min_x <- min(min(x1), min(x2))
    max_x <- max(max(x1), max(x2))

    d_y1 <- get_density(x1, n = n, from = min_x, to = max_x)
    d_y2 <- get_density(x2, n = n, from = min_x, to = max_x)

    OVL <- d_y1 %>% dplyr::mutate(y1 = y) %>% left_join(d_y2 %>% mutate(y2 = y), by = "x") %>%
        dplyr::select(x, y1, y2) %>% group_by(x) %>%
        summarise(OVL = min(y1, y2), OVA = max(y1, y2), .groups = "drop") %>% ungroup() %>%
        summarise(total = sum(OVL) / sum(OVA), .groups = "drop") %>%
        unlist(use.names = FALSE)

    return(OVL)
}
