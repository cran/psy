\name{sphpca}
\alias{sphpca}
\title{Spherical Representation of a Correlation Matrix}
\description{
Graphical representation of a correlation matrix, similar to principal component analysis (PCA) but the mapping is on a sphere. The information is close to a 3d PCA, the picture is however easier to interpret since the variables are in fact on a 2d map.
}
\usage{
sphpca(datafile, h=0, v=0, f=0, cx=0.75, nbsphere=2, back=FALSE, input="data",
       method="approx", maxiter=500, output=FALSE)
}
\arguments{
  \item{datafile}{name of datafile}
  \item{h}{rotation of the sphere on a horizontal plane (in degres)}
  \item{v}{rotation of the sphere on a vertical plane (in degres)}
  \item{f}{rotation of the sphere on a frontal plane (in degres)}
  \item{cx}{size of the lettering (0.75 by default, 1 for bigger letters, 0.5 for smaller)}
  \item{nbsphere}{two by default: front and back}
  \item{back}{"FALSE" by default: the back sphere is not seen through}
  \item{input}{"data" by default: raw data are analysed, if not "data": correlation matrix is expected}
  \item{method}{"approx" by default: the estimation is based on a principal component analysis approximation. If "exact" the "approx" estimation is optimized (may be computationaly consumming). if "rscal" a multidimensional scaling approach is used: distances between points on the sphere are optimized so that they represent at best the original correlations. The scaling that is used leads to angles on the sphere proportional to correlation between variables}
  \item{maxiter}{maximum number of iterations in the optim process}
  \item{output}{FALSE by default: if TRUE and method="rscal" numerical results are proposed}
}
\details{
There is an isophormism between a correlation matrix and points on the unit hypersphere of Rn. It can be shown that a 3d spherical representation of a correlation matrix is statistically and cognitively interesting (see reference).
The default option method="approx" is based on a principal components approximation (see reference). It is fast and gives rather good results.
If method="exact" the representation is sligthly improved in terms of fit (the sphere minimizes the sum of squared distances between the original variables on the hypersphere and their projections on the sphere).
The option method="rscal" optimizes the representation of correlations between variables with distances between points (in a least squares sense). For convenience, the scaling of points on the sphere is chosen so that angles between points are linearly related to correlations between variables (this is not the case on the hypersphere were d=[2*(1-r)]^0.5).
For method="exact" or method="rscal" computations may be rather lengthy (and not sensible for more than 20-40 variables).
The sphere may be rotated to help in visualising most of variables on a same side (front for example).
By default, the back of the sphere (right plot) is not seen showing through.
}
\value{
A plot.
If method="rscal" and output=TRUE, a list with :
  \item{$stress.before.optim}{Stress before optimization. The stress is equal to the sum of squares of differences between distances on the 3d sphere and distances on the hypersphere.}
  \item{$stress.after.optim}{Stress after optimization.}
  \item{$convergence}{If 0, convergence is OK. If not, maxiter may be increased.}
  \item{$correlations}{Correlation matrix of variables (Pearson).}
  \item{$residuals}{Differences between observed correlations (hypersphere) and correlations estimated from points on the 3d sphere.}
  \item{$mean.abs.resid}{Mean of absolute values of residuals.}
}
\references{Falissard B, A spherical representation of a correlation matrix, Journal of Classification (1996), 13:2, 267-280.}
\author{Bruno Falissard}
\examples{
data(sleep)
sphpca(sleep[,c(2:5,7:11)])
## spherical representation of ecological and constitutional correlates in mammals

sphpca(sleep[,c(2:5,7:11)],method="rscal",output=TRUE)
## idem, but optimizes the representation of correlations between variables with distances
## between points

corsleep <- as.data.frame(cor(sleep[,c(2:5,7:11)],use="pairwise.complete.obs"))
sphpca(corsleep,input="Cor")
sphpca(corsleep,method="rscal",input="Cor")
## when missing data are numerous, the representation of a pairwise correlation
## matrix may be preferred (even if mathematical properties are not so good...)

sphpca(corsleep,method="rscal",input="Cor",h=180,f=180,nbsphere=1,back=TRUE)
## other option of presentation

##
# library(polycor)
# sleep$Predation <- as.ordered(sleep$Predation)
# sleep$Sleep.exposure <- as.ordered(sleep$Sleep.exposure)
# sleep$Danger <- as.ordered(sleep$Danger)
# corsleeph <- as.data.frame(hetcor(sleep[,c(2:5,7:11)])$correlations)
# sphpca(corsleeph,input="Cor",f=180)
# sphpca(corsleeph,method="rscal",input="Cor",f=180)
## --> Correlations between discrete variables may appear shoking to some statisticians (?)
## --> Representation of polychoric/polyserial correlations could be prefered in this situation
}
\keyword{multivariate}
