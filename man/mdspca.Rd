\name{mdspca}
\alias{mdspca}
\title{Graphical representation of a correlation matrix using a Principal Component Analysis}
\description{
Similar to many routines, the interest is in the possible representation of both variables and subjects (and by the way categorical variables) with active and supplementary points. Missing data are omitted.
}
\usage{
mdspca(datafile, supvar="no", supsubj="no", namesupvar=colnames(supvar,do.NULL=FALSE), namesupsubj=colnames(supsubj, do.NULL=FALSE), dimx=1, dimy=2, cx=0.75)
}
\arguments{
  \item{datafile}{name of datafile}
  \item{supvar}{matrix corresponding to supplementary variables (if any), supvar="no" by default}
  \item{supsubj}{matrix corresponding to supplementary subjects (if any), supsubj="no" by default}
  \item{namesupvar}{names of the points corresponding to the supplementary variables}
  \item{namesupsubj}{names of the points corresponding to the supplementary subjects}
  \item{dimx}{rank of the component displayed on the x axis (1 by default)}
  \item{dimy}{rank of the component displayed on the y axis (2 by default)}
  \item{cx}{size of the lettering (0.75 by default, 1 for bigger letters, 0.5 for smaller)}
}

\value{
A diagram (two diagrams if supplementary subjects are used)
}
\author{Bruno Falissard}
\examples{
data(sleep)

mdspca(sleep[,c(2:5,7:11)])
## three consistent groups of variables, paradoxical sleep (in other words: dream)
## is negatively correlated with danger

mdspca(sleep[,c(2:5,7:11)],supvar=sleep[,6],namesupvar="Total.sleep",supsubj=sleep[,1],namesupsubj="",cx=0.5)
## Total.sleep is here a supplementary variable since it is deduced
## from Paradoxical.sleep and Slow.wave.sleep
## The variable Species is displayed in the subject plane,
## Rabbit and Cow have a high level of danger
}
\keyword{multivariate}
