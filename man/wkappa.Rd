\name{wkappa}
\alias{wkappa}
\title{weighted Kappa for 2 raters}
\description{
Computes a weighted Kappa for agreement in the case of 2 raters. The diagnosis (the object of the rating) may have k possible ordered values.
}
\usage{
wkappa(r,weights="squared")
}
\arguments{
  \item{r}{n*2 matrix or dataframe, n subjects and 2 raters}
  \item{weights}{weights="squared" to obtain squared weights. If not, absolute weights are computed (see details)}
}
\details{
The diagnoses are ordered as follow: numbers < letters, letters and numbers ordered naturally.
For weigths="squared", weights are related to squared differences between rows and columns indices (in this situation wkappa is close to an icc).
For weights!="squared", weights are related to absolute values of differences between rows and columns indices.
The function deals with the case where the two raters have not exactly the same scope of rating (some software associate an error with this situation).
Missing value are omitted.
}
\value{
A list with :
  \item{$table}{the 2*k table of raw data (first rater in rows, second rater in columns)}
  \item{$weights}{"squared" or "absolute"}
  \item{$kappa}{Weighted Kappa}
}
\references{Cohen, J. Weighted kappa: nominal scale agreement with provision for scaled disagreement or partial credit. Psychological Bulletin 70 (1968): 213-220.
}
\author{Bruno Falissard}
\examples{
data(expsy)
wkappa(expsy[,c(11,13)])        # weighted kappa (squared weights)

library(boot)
wkappa.boot <- function(data,x) {wkappa(data[x,])[[3]]}
res <- boot(expsy[,c(11,13)],wkappa.boot,1000)
quantile(res$t,c(0.025,0.975))  # two-sided bootstrapped confidence interval of weighted kappa
}
\keyword{univar}

