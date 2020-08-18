## Implemented by Seongyong Park (2020-08-18)
## Overlap calculation by distribution fitting

ovl_distfit <- function(x1, x2, family=c("norm", "lognorm")){

  x <- range(min(x1, x2), max(x1, x2))
  len_x = min(length(x1), length(x2))
  x <- seq(x[1], x[2], length.out=1+log2(len_x))

  fit_t <- fitdist(x1, family)
  fit_b <- fitdist(x2, family)

  pdf_t = hist(x1, x, plot=FALSE)
  pdf_b = hist(x2, x, plot=FALSE)
  pd_t <- pnorm(x, fit_t$estimate[1], fit_t$estimate[2])
  pd_b <- pnorm(x, fit_b$estimate[1], fit_b$estimate[2])

  OVL <- sum(apply(cbind(diff(pd_b), diff(pd_t)), 1, min))

  return (OVL)
}
