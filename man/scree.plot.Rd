\name{scree.plot}
\alias{scree.plot}
\title{Screeplot of eigenvalues, simulated data are available}
\description{
Graphical representation of the eigenvalues of a correlation/covariance matrix.
Usefull to determine the dimensional structure of a set of variables.
Simulation are proposed to help the interpretation.
}
\usage{
scree.plot(namefile, title = "Scree Plot", type = "R", use = "complete.obs", simu = "F")
}
\arguments{
  \item{namefile}{dataset}
  \item{title}{Title}
  \item{type}{ type="R" to obtain the eigenvalues of the correlation matrix of dataset, type="V" for the covariance matrix, type="M" if the input data is directly the matrix, type="E" if the input data are directly the eigenvalues}
  \item{use}{omit missing values by default, use="P" to analyse the pairwise correlation/covariance matrix}
  \item{simu}{simu=p to add p screeplots of simulated random normal data (same number of patients and variables as in the original data set, same pattern of missing data if use="P")}
}
\details{
Simulations lead sometimes to underestimate the actual number of dimensions (as opposed to Kayser rule: eigen values superior to 1). Basically, simu=20 is enough.
}
\value{
a plot
}
\references{Horn, JL (1965) A Rationale and Test for the Number of Factors in Factor Analysis, Psychometrika, 30, 179-185.
Cattell, RB (1966) The scree test for the number of factors. Multivariate Behavioral Research, 1, 245-276.}
\author{Bruno Falissard}
\examples{
data(expsy)
scree.plot(expsy[,1:10],simu=20,use="P") #no obvious structure with such a small sample
}
\keyword{multivariate}

