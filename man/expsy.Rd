\name{expsy}
\alias{expsy}
\non_function{}
\title{Data set related to psychometry}
\usage{data(expsy)}
\description{
The \code{expsy}
data frame has 30 rows and 16 columns with missing data.
it1-it10 correspond to the rating of 30 patients with a 10 items scale.
r1, r2, r3 to the rating of item 1 by 3 different clinicians of the same 30 patients.
rb1, rb2, rb3 to the binary transformation of r1, r2, r3 (1 or 2 -> 0; and 3 or 4 -> 1) .
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{it1}{a numeric vector corresponding to item 1 (rated from 1:low to 4:high)}
    \item{it2}{a numeric vector corresponding to item 2 (rated from 1:high to 4:low)}
    \item{it3}{a numeric vector corresponding to item 3 (rated from 1:low to 4:high)}
    \item{it4}{a numeric vector corresponding to item 4 (rated from 1:low to 4:high)}
    \item{it5}{a numeric vector corresponding to item 5 (rated from 1:low to 4:high)}
    \item{it6}{a numeric vector corresponding to item 6 (rated from 1:low to 4:high)}
    \item{it7}{a numeric vector corresponding to item 7 (rated from 1:low to 4:high)}
    \item{it8}{a numeric vector corresponding to item 8 (rated from 1:low to 4:high)}
    \item{it9}{a numeric vector corresponding to item 9 (rated from 1:low to 4:high)}
    \item{it10}{a numeric vector corresponding to item 10 (rated from 1:low to 4:high)}
    \item{r1}{a numeric vector corresponding to item 1 rated by rater 1}
    \item{rb1}{binary transformation of r1}
    \item{r2}{a numeric vector corresponding to item 1 rated by rater 2}
    \item{rb2}{binary transformation of r2}
    \item{r3}{a numeric vector corresponding to item 1 rated by rater 3}
    \item{rb3}{binary transformation of r3}
  }
}
\source{
artificial data}
\examples{
data(expsy)
expsy[1:4,]
}
\keyword{datasets}
