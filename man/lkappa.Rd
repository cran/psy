\name{lkappa}
\alias{lkappa}
\title{Light's kappa for n raters}
\description{
Computes Light's Kappa for agreement in the case of n raters. The diagnosis (the object of the rating) may have k possible values (ordered or not).
}
\usage{
lkappa(r, type="Cohen", weights="squared")
}
\arguments{
  \item{r}{m*n matrix, m subjects and n raters}
  \item{type}{type="Cohen" for a categorical diagnosis. If not, the diagnosis is supposed to be ordered}
  \item{weights}{weights="squared" for a weighted kappa with squared weights. If not, weigths are computed with absolute values.}
}
\details{
Light's Kappa is equal to the mean of the n(n-1)/2 kappas obtained from each pair of raters. Missing values are omitted locally when considering each pair of raters.
If type="Cohen" the diagnosis is considered as a categorical variable. If not, the diagnosis is considered as an ordered variable and weigthed kappa's are computed.
In this last situation, the type of weights that is used (squared or absolute values) is given by the variable weigths.
}
\value{
kappa (mean of the n(n-1)/2 kappas obtained from each pair of raters)
}
\references{
Conger, A.J. (1980), Integration and generalisation of Kappas for multiple raters, Psychological Bulletin, 88, 322-328.
}
\author{Bruno Falissard}
\examples{
data(expsy)
lkappa(expsy[,c(11,13,15)])       # Light's kappa for non binary diagnosis
lkappa(expsy[,c(12,14,16)])       # Light's kappa for binary diagnosis
lkappa(expsy[,c(11,13,15)], type="weighted")      # Light's kappa for non binary ordered diagnosis

library(boot)
lkappa.boot <- function(data,x) {lkappa(data[x,], type="weighted")}
res <- boot(expsy[,c(11,13,15)],lkappa.boot,1000)
quantile(res$t,c(0.025,0.975))    # Bootstrapped confidence interval of Light's kappa
boot.ci(res,type="bca")           # adjusted bootstrap percentile (BCa) confidence interval (better)
}
\keyword{univar}

