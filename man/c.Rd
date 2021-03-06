% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bb_params.R
\name{c}
\alias{c}
\title{Combine multiple bb_params objects into a vector}
\usage{
c(..., recursive = FALSE)
}
\arguments{
\item{...}{\link[BentoBox]{bb_params} objects to be concatenated.}

\item{recursive}{logical. If \code{recursive = TRUE}, the function
recursively descends through lists
(and pairlists) combining all their elements into a vector.}
}
\value{
\code{NULL} or an expression or a vector of an appropriate mode.
(With no arguments the value is \code{NULL}.)
}
\description{
Combine multiple bb_params objects into a vector
}
\examples{
## Define parameters
p1 <- bb_params(chrom = "chr1", assembly = "hg19")

## Define another set of parameters
p2 <- bb_params(fontsize = 10, assembly = "hg19")

## Combine parameters into one `bb_params` object
pTotal <- c(p1, p2)
}
