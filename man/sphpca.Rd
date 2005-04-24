\name{sphpca}
\alias{sphpca}
\title{Spherical Representation of a Correlation Matrix}
\description{
Graphical representation of a correlation matrix, similar to principal component analysis (PCA) but the mapping is on a sphere. The information is close to a 3d PCA, the picture is however easier to interpret since the variables are in fact on a 2d map.
}
\usage{
sphpca(datafile, h=0, v=0, f=0, cx=0.75, nbsphere=2, back=FALSE)
}
\arguments{
  \item{datafile}{name of datafile}
  \item{h}{rotation of the sphere on a horizontal plane (in degres)}
  \item{v}{rotation of the sphere on a vertical plane (in degres)}
  \item{f}{rotation of the sphere on a frontal plane (in degres)}
  \item{cx}{size of the lettering (0.75 by default, 1 for bigger letters, 0.5 for smaller)}
  \item{nbsphere}{two by default: front and back}
  \item{back}{"FALSE" by default: the back sphere is not seen through}
}
\details{
The sphere may be rotated to help in visualising most of variables on a same side (front for example).
By default, the back of the sphere (right plot) is not seen showing through. Computations are based
on a principal components approximation (see reference for details).
}
\value{
A plot
}
\references{Falissard B, A spherical representation of a correlation matrix, Journal of Classification (1996), 13:2, 267-280.}
\author{Bruno Falissard}
\examples{
data(sleep)
sphpca(sleep[,c(2:5,7:11)])
##spherical representation of ecological and constitutional correlates in mammals
}
\keyword{multivariate}

