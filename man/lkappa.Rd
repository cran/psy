\name{lkappa}
\alias{lkappa}
\title{Light's kappa for n raters}
\description{
Computes Light's Kappa for agreement in the case of n raters. The diagnosis (the object of the rating) may have k possible values.
}
\usage{
lkappa(r)
}
\arguments{
  \item{r}{m*n matrix, m subjects and n raters}
}
\details{
Light's Kappa is equal to the mean of the n(n-1)/2 Cohen's kappas obtained from each pair of raters. Missing values are omitted locally when considering each pair of raters.
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
lkappa(expsy[,c(11,13,15)])
lkappa(expsy[,c(12,14,16)])
}
\keyword{univar}
