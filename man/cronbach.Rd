\name{cronbach}
\alias{cronbach}
\title{Cronbach's coefficient alpha}
\description{
Computes the Cronbach's reliability coefficient alpha. This coefficient may be applied to a series of items destinated to be aggregated in a single score. It estimates reliability in the framework of the domain sampling model.
}
\usage{
cronbach(v1)
}
\arguments{
  \item{v1}{n*p matrix or dataframe, n subjects and p items}
}
\details{
Missing value are omitted in a "listwise" way (all items are removed even if only one of them is missing).
}
\value{
A list with :
  \item{$sample.size}{Number of subjects under study}
  \item{$number.of.items}{Number of items of the scale or questionnaire}
  \item{$alpha}{alpha}
}
\references{Nunnaly, J.C., Bernstein, I.H. (1994), Psychometric Theory, 3rd edition, McGraw-Hill Series in Psychology.}
\author{Bruno Falissard}
\examples{
data(expsy)
cronbach(expsy[,1:10])  ## not good because item 2 is reversed (1 is high and 4 is low)
cronbach(cbind(expsy[,c(1,3:10)],-1*expsy[,2]))  ## better

datafile <- cbind(expsy[,c(1,3:10)],-1*expsy[,2])
library(boot)
cronbach.boot <- function(data,x) {cronbach(data[x,])[[3]]}
res <- boot(datafile,cronbach.boot,1000)
quantile(res$t,c(0.025,0.975))  ## two-sided bootstrapped confidence interval of Cronbach's alpha
}
\keyword{univar}
