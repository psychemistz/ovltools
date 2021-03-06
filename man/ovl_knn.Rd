% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/knn.R
\name{ovl_knn}
\alias{ovl_knn}
\title{Calculate distribution overlap by KNN}
\usage{
ovl_knn(x1, x2, k = 5)
}
\arguments{
\item{x1}{A numeric vector.}

\item{x2}{A numeric vector.}

\item{k}{An integer. Number of k in KNN}
}
\value{
estimated overlap between two distributions (range: 0 to 1)
}
\description{
Calculate distribution overlap by KNN (K-Nearest neighbor) This function compares label of K-nearest neighbors and
calculate how many labels are mismatch.
}
\examples{
x1 = rnorm(100, 0)
x2 = rnorm(100, 1)
ovl_knn(x1, x2, k=5)
}
\references{
This code is adopted from AmRMR
(\href{https://www.mdpi.com/2073-431X/8/2/42}{paper}) and modified by
Seongyong Park. You can find their implementation in their websites.
(\href{https://bitldku.github.io/home/sw/R-value.html}{Java},
\href{https://bitldku.github.io/home/sw/AmRMR.html}{R}) Many thanks to the
professor Sejong Oh (Dankook University, \email{sejongoh@dankook.ac.kr}) who kindly share his work.
}
\author{
Seongyong Park (2020-08-18)
}
