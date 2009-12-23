\name{icc}
\alias{icc}
\title{Intraclass correlation coefficient (ICC)}
\description{
Computes the ICC of several series of measurements, for example in an interrater agreement study. Two types of ICC are proposed: consistency and agreement.
}
\usage{
icc(data)
}
\arguments{
  \item{data}{n*p matrix or dataframe, n subjects p raters}
}
\details{
Missing data are omitted in a listwise way. The "agreement" ICC is the ratio of the subject variance by the sum of the subject variance, the rater variance and the residual; it is generally prefered. The "consistency" version is the ratio of the subject variance by the sum of the subject variance and the residual; it may be of interest when estimating the reliability of pre/post variations in measurements.
}
\value{
A list with :
  \item{$nb.subjects}{number of subjects under study}
  \item{$nb.raters}{number of raters}
  \item{$subject.variance}{subject variance}
  \item{$rater.variance}{rater variance}
  \item{$residual}{residual variance}
  \item{$icc.consistency}{Intra class correlation coefficient, "consistency" version}
  \item{$icc.agreement}{Intra class correlation coefficient, "agreement" version}
}
\references{Shrout, P.E., Fleiss, J.L. (1979), Intraclass correlation: uses in assessing rater reliability, Psychological Bulletin, 86, 420-428.}
\author{Bruno Falissard}

\examples{
data(expsy)
icc(expsy[,c(12,14,16)])

library(boot)
icc.boot <- function(data,x) {icc(data[x,])[[7]]}
res <- boot(expsy[,c(12,14,16)],icc.boot,1000)
quantile(res$t,c(0.025,0.975))  # two-sided bootstrapped confidence interval of icc (agreement)
boot.ci(res,type="bca")         # adjusted bootstrap percentile (BCa) confidence interval (better)
}
\keyword{univar}

