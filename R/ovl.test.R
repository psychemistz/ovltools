#' Permutation test to assess significance of distribution overlap
#'
#' @param x1 A numeric vector.
#' @param x2 A numeric vector.
#' @param method method to calculate distribution overlap
#' @param nperm An integer. Number of permutation performed. default is 1000.
#' @param seed A logical value. default is TRUE. set seed for reproducibility.
#' @return test statistic (overlap) and permutation p-value
#' @examples
#' x1 = rnorm(100, 0)
#' x2 = rnorm(100, 1)
#' ovl.test(x1, x2, method = "hist", nperm = 1000)
#' @export
#' @description
#' Assess significance of distribution overlap.
#' User need to specify which method to use to calculate distribution overlap.
#' This function test whether given two samples are drawn from the same distribution or not. i.e.,
#' H0: complete overlap. Two distributions are the same. E(OVL) = 1.
#' H1: Two distributions are different.
#' @author Seongyong Park (2020-08-18)

ovl.test <- function(x1, x2, method = c("hist", "distfit", "knn", "kde"), nperm=1000, seed=TRUE){

  dataset = data.frame(x=c(x1, x2), cl=c(rep(0,length(x1)), rep(1,length(x2))))

  if (seed){
    set.seed(101) ## for reproducibility
  }

  nperm <- nperm
  res <- numeric(nperm) ## set aside space for results
  for (i in 1:nperm) {
    ## standard approach: scramble response value
    perm <- sample(nrow(dataset))
    bdat <- transform(dataset, cl=cl[perm])

    ## compute & store difference in means; store the value
    if (method == "knn"){
      res[i] <- ovl_knn(bdat[bdat$cl==1,"x"], bdat[bdat$cl==0,"x"])

    } else if (method == "kde"){
      res[i] <- ovl_kde(bdat[bdat$cl==1,"x"], bdat[bdat$cl==0,"x"])

    } else if (method == "hist"){
      res[i] <- ovl_hist(bdat[bdat$cl==1,"x"], bdat[bdat$cl==0,"x"])

    } else if (method == "distfit"){
      res[i] <- ovl_distfit(bdat[bdat$cl==1,"x"], bdat[bdat$cl==0,"x"])

    } else {
      message("Please choose method within knn, kde, hist and distfit")

    }

  }

  if (method == "knn"){
    obs <- ovl_knn(dataset[dataset$cl==1,"x"], dataset[dataset$cl==0,"x"])

  } else if (method == "kde"){
    obs <- ovl_kde(dataset[dataset$cl==1,"x"], dataset[dataset$cl==0,"x"])

  } else if (method == "hist"){
    obs <- ovl_hist(dataset[dataset$cl==1,"x"], dataset[dataset$cl==0,"x"])

  } else if (method == "distfit"){
    obs <- ovl_distfit(dataset[dataset$cl==1,"x"], dataset[dataset$cl==0,"x"])

  } else {
    message("Please choose method within knn, kde, hist and distfit")

  }

  ## append the observed value to the list of results
  res <- c(res,obs)

  ## p-value by permutation test
  ## H0: complete overlap. Two distributions are the same. E(OVL) = 1.
  ## H1: Two distributions are different.
  pval = mean(abs(res)<=abs(obs))

  return(c(OVL=obs, pval=pval))

}
