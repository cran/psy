\name{sleep}
\alias{sleep}
\non_function{}
\title{ Ecological and Constitutional Data in Mammals }
\usage{data(sleep)}
\description{
Data from which conclusions were drawn in the article 
Mammals: Ecological and Constitutional Correlates,
by Allison, T. and Cicchetti, D. (1976)
Science, November 12, vol. 194, pp.732-734
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{Species}{a factor with levels}
    \item{Body.weight}{a numeric vector, body weight in kg}
    \item{Brain.weight}{a numeric vector, Brain weight in g}
    \item{Slow.wave.sleep}{a numeric vector, “nondreaming” sleep (hrs/day)}
    \item{Paradoxical.sleep}{a numeric vector, “dreaming” sleep (hrs/day)}
    \item{Total.sleep}{a numeric vector, “nondreaming” + “dreaming” (hrs/day)}
    \item{Maximum.life.span}{a numeric vector (in years)}
    \item{Gestation.time}{a numeric vector (in days)}
    \item{Predation}{a numeric vector, Predation index (1 min - 5 max)}
    \item{Sleep.exposure}{a numeric vector, Sleep exposure index (1 min - 5 max)}
    \item{Danger}{a numeric vector, Overall danger index (1 min - 5 max)}
  }
}
\source{
http://lib.stat.cmu.edu/datasets/sleep
}
\references{
Mammals: Ecological and Constitutional Correlates, by Allison, T. and Cicchetti, D. (1976)
Science, November 12, vol. 194, pp.732-734
}
\examples{
data(sleep)
str(sleep)
}
\keyword{datasets}
