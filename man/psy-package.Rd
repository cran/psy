\name{psy}
\alias{psy}
\docType{package}
\title{Various procedures used in psychometry}
\description{Kappa, Intra class correlation coefficient, Cronbach alpha, Scree plot,
Multitraits multimethods, Spherical representation of a correlation matrix}
\details{
\tabular{ll}{
Package: \tab psy\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2009-12-23\cr
License: \tab free\cr
LazyLoad: \tab yes\cr
}
}
\author{Bruno Falissard <falissard_b@wanadoo.fr>}
\references{
Falissard B, A spherical representation of a correlation matrix, Journal of Classification (1996), 13:2, 267-280.\cr
Horn, JL (1965) A Rationale and Test for the Number of Factors in Factor Analysis, Psychometrika, 30, 179-185.\cr
Mammals: Ecological and Constitutional Correlates, by Allison, T. and Cicchetti, D. (1976)
Science, November 12, vol. 194, pp.732-734\cr
Jouvent, R et al 1988 La clinique polydimensionnelle de humeur depressive.
Nouvelle version echelle EHD  : Polydimensional rating scale of depressive mood.
Psychiatrie et Psychobiologie.
}
\keyword{package}
\examples{
data(sleep)
sphpca(sleep[,c(2:5,7:11)])
data(expsy)
scree.plot(expsy[,1:10],simu=20,use="P")
data(ehd)
par(mfrow=c(1,5))
mtmm(ehd,list(c("e15","e18","e19","e20"),c("e4","e5","e6","e14","e17"),c("e11","e13","e16")
,c("e1","e10","e12"),c("e2","e3","e7","e8","e9")))
}

