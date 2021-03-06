% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transmat.R
\name{transmat}
\alias{transmat}
\title{transmat: Cross-wave transition matrix}
\usage{
transmat(pre_test_var, pst_test_var, subgroup = NULL, force9 = FALSE)
}
\arguments{
\item{pre_test_var}{Required. A vector carrying pre-test scores of a particular item. Only}

\item{pst_test_var}{Required. A vector carrying post-test scores of a particular item}

\item{subgroup}{Optional. A Boolean vector indicating rows of the relevant subset.}

\item{force9}{Optional. There are cases where DK data doesn't have DK. But we need the entire matrix. By default it is FALSE.}
}
\value{
a numeric vector. 
Assume 1 denotes correct answer, 0 and NA incorrect, and d 'don't know.'
When there is no don't know option and no missing, the entries are: x00, x10, x01, x11
When there is a don't know option, the entries of the vector are: x00, x10, xd0, x01, x11, xd1, xd0, x1d, xdd
}
\description{
Prints Cross-wave transition matrix and returns the vector behind the matrix.  
Missing values are treated as ignorance. Don't know responses need to be coded as 'd'.
}
\examples{
pre_test_var <- c(1,0,0,1,0,1,0)
pst_test_var <- c(1,0,1,1,0,1,1)
transmat(pre_test_var, pst_test_var)

# With NAs
pre_test_var <- c(1,0,0,1,"d","d",0,1,NA)
pst_test_var <- c(1,NA,1,"d",1,0,1,1,"d") 
transmat(pre_test_var, pst_test_var)
}

