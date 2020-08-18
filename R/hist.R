## This code is adopted from GCNR paper. The original code was matlab but I implemented it in the R. Reference: Rodriguez-Molares et. al,
## 2019. The generalized contrast-to-noise ratio: a formal definition for lesion detectability. IEEE Transactions on Ultrasonics,
## Ferroelectrics, and Frequency Control Codebase: No code base is available. please contact to <alfonso.r.molares@ntnu.no>

## reimplemented by Seongyong Park (2020-08-18) The original implementation used fixed sized bin (n=100) but I used adaptive bin size
## selection method proposed in C. J. Cellucci, A. M. Albano, and P. E. Rapp, <U+653C><U+3E32>\u0080?Statistical validation of mutual information
## calculations: Comparison of alternative numerical algorithms,<U+653C><U+3E32>\u0080<U+393C><U+3E64> Physical Review E, vol. 71, no. 6, p. 066208, 2005.

ovl_hist <- function(x1, x2) {

    x <- range(min(x1, x2), max(x1, x2))
    len_x <- min(length(x1), length(x2))
    x <- seq(x[1], x[2], length.out = 1 + log2(len_x))

    pdf_t <- hist(x1, x, plot = FALSE)
    pdf_b <- hist(x2, x, plot = FALSE)

    ovl <- sum(apply((rbind(pdf_t$counts / sum(pdf_t$counts), pdf_b$counts / sum(pdf_b$counts))), 2, min))

    return(ovl)
}
