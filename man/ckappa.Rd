\name{ckappa}
\alias{ckappa}
\title{Cohen's Kappa for 2 raters}
\description{
Computes Cohen's Kappa for agreement in the case of 2 raters. The diagnosis (the object of the rating) may have k possible values.
}
\usage{
ckappa(r)
}
\arguments{
  \item{r}{n*2 matrix or dataframe, n subjects and 2 raters}
}
\details{
The function deals with the case where the two raters have not exactly the same scope of rating (some software associate an error with this situation).
Missing value are omitted.
}
\value{
A list with :
  \item{$table}{the 2*k table of raw data (first rater in rows, second rater in columns)}
  \item{$kappa}{Cohen's Kappa}
}
\references{Cohen, J. (1960), A coefficient of agreement for nominal scales, Educational and Psychological measurements, 20, 37-46.
}
\author{Bruno Falissard}
\examples{
data(expsy)
ckappa(expsy[,c(12,14)])
ckappa(expsy[,c(11,13)])
}
\keyword{univar}