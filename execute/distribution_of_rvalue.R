## Distribution of Rvalue
library(dplyr)
library(tidyr)
library(ggplot2)
library(fitdistrplus)
## Deviation Experiments
distfit = histogram = kde = rv = ks = c()
dx = seq(-5, 5, 0.01)
for(i in 1:length(dx)){
  x1 = rnorm(100)
  x2 = x1+dx[i]
  tmp_rv = ovl_knn(x1, x2, k=5)
  tmp_kde = ovl_kde(x1, x2)
  tmp_hist = ovl_hist(x1, x2)
  tmp_ks = 1-ks.test(x1,x2)$statistic
  tmp_distfit = ovl_distfit(x1, x2, "norm")
  rv = c(rv, tmp_rv)
  kde = c(kde, tmp_kde)
  histogram = c(histogram, tmp_hist)
  distfit = c(distfit, tmp_distfit)
  ks = c(ks, tmp_ks)
}

## Theoretical value
cum_df = 2 * pnorm(-(abs(dx-0))/(2*1))
prob_df = 2 * dnorm(-(abs(dx-0))/(2*1))


minmax_norm <- function(v){
  return((v-min(v))/(max(v)-min(v)))
}

RvMat = data.frame(dx, prob_df, cum_df, rv, kde, histogram, distfit, ks)
RvMat %>% dplyr::select(dx, cum_df, histogram, ks) %>% gather(method, value, -dx) %>% ggplot(aes(x=dx, y=value, col=method)) +
  geom_point() +
  theme_bw() +
  xlab("Deviation") +
  ylab("value")



## Random Sample Experiments
library(overlapping)
distfit = histogram = kde = kde_by_package = rv_from_src = ks = rv = c()
for(i in 1:1000){
  x1 = rnorm(1000)
  x2 = x1 + 1
  tmp_rv = ovl_knn(x1, x2, k=5)
  tmp_kde = ovl_kde(x1, x2)
  tmp_hist = ovl_hist(x1, x2)
  tmp_distfit = ovl_distfit(x1, x2, "norm")
  tmp_ks = 1-ks.test(x1,x2)$statistic
  rv = c(rv, tmp_rv)
  kde = c(kde, tmp_kde)
  kde_by_package = c(kde_by_package, overlap(list(x1, x2))$OV)
  rv_from_src = c(rv_from_src, Rvalue(c(x1,x2), c(rep(0,length(x1)), rep(1,length(x2)))))
  histogram = c(histogram, tmp_hist)
  distfit = c(distfit, tmp_distfit)
  ks = c(ks, tmp_ks)
}

## Theoretical value
cum_df = 2 * pnorm(-(abs(1-0))/(2*1))
prob_df = 2 * dnorm(-(abs(1-0))/(2*1))

RvMat = data.frame(prob_df, cum_df, rv, rv_from_src, distfit, kde, kde_by_package, histogram, ks)
RvMat %>% gather(method, value) %>% ggplot(aes(x=method, y=value, col=method)) +
  geom_violin() +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  theme_bw() +
  xlab("methods") +
  ylab("value")


## Save all figures in this session
plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE);
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to="C:/Users/sypark/Desktop/5개인Project/2020-0GSSMD-HTS/3results/comparison_with_other_pkgs")
