## This code is adopted from AmRMR paper.
## Reference: 2019, Insik Jo, Improved Measures of Redundancy and Relevance for mRMR Feature Selection.
## Codebase: https://bitldku.github.io/home/sw/R-value.html
## Many thanks to the professor Sejong Oh (Kunkook University) who kindly share his work.

## Modified by Seongyong Park (2020-08-18)
## Rvalue provides estimated overlap between two classes or more. Here I assumed only two classes.

ovl_knn <- function(x1, x2, k=5){

  rvalue = 0
  ds = cbind(c(x1, x2)) ## data matrix
  cl = cbind(c(rep(0, length(x1)), rep(1, length(x2)))) ## class vector (column vector)

  noRow = nrow(ds)           # no of Row
  knns = get.knn(ds, k)      # find k nearest neighbors for all instances (FNN package)
  knns.ndx <- data.frame(knns[["nn.index"]])

  inOverlap = c()
  diffNeighbor  = 0;
  for (i in 1:noRow) {
    thisClass = cl[i]
    for (j in 1:k) {
      if (cl[knns.ndx[i,j]] != thisClass) {
        diffNeighbor = diffNeighbor + 1
      }
    }
  }

  rvalue =  diffNeighbor/(noRow*k)
  return(rvalue)
}

## Generalized version of Rvalue. Here ds denotes data or feature matrix and cl denotes class vector.
Rvalue <- function (ds, cl, k=5)
{
  rvalue = 0
  if (is.vector(ds)) {
    ds = cbind(ds)
  }
  if (is.vector(cl)) {
    cl = cbind(cl)
  }

  noRow = nrow(ds)           # no of Row

  knns = get.knn(ds, k)      # find k nearest neighbors for all instances
  knns.ndx <- data.frame(knns[["nn.index"]])

  inOverlap = c()

  diffNeighbor  = 0;
  for (i in 1:noRow) {
    thisClass = cl[i]
    for (j in 1:k) {
      if (cl[knns.ndx[i,j]] != thisClass) {
        diffNeighbor = diffNeighbor + 1
      }
    }
  }

  rvalue =  diffNeighbor/(noRow*k)
  return(rvalue)
}




