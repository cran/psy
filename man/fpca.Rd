\name{fpca}
\alias{fpca}
\title{Focused Principal Components Analysis}
\description{
Graphical representation similar to a principal components analysis but adapted to data structured with dependent/independent variables
}
\usage{
fpca(datafile, y, x, cx=0.75, namesvar=attributes(datafile)$names, pvalues="No", partial="Yes", input="data", contraction="No", sample.size=1)
}
\arguments{
  \item{datafile}{name of datafile}
  \item{y}{column number of the dependent variable}
  \item{x}{column numbers of the independent (explanatory) variables}
  \item{cx}{size of the lettering (0.75 by default, 1 for bigger letters, 0.5 for smaller)}
  \item{namesvar}{label of variables (names of columns by default)}
  \item{pvalues}{vector of prespecified pvalues (pvalues="No" by default) (see below)}
  \item{partial}{partial="Yes" by default, corresponds to the original method (see below)}
  \item{input}{input="Cor" for a correlation matrix (input="data" by default)}
  \item{contraction}{change the aspect of the diagram, contraction="Yes" is convenient for large data set (contraction="No" by default)}
  \item{sample.size}{to be specified if input="Cor"}
}
\details{
This representation is close to a Principal Components Analysis (PCA).
Contrary to PCA, correlations between the dependent variable and the other variables are represented faithfully. The relationships between non dependent variables are interpreted like in a PCA: correlated variables are close or diametrically opposite (for negative correlations), independent variables make a right angle with the origin.
The focus on the dependent variable leads formally to a partialisation of the correlations between the non dependent variables by the dependent variable (see reference). To avoid this partialisation, the option partial="No" can be used.
It may be interesting to represent graphically the strength of association between the dependent variable and the other variables using p values coming from a model. A vector of pvalue may be specified in this case.
}
\value{
A plot.
}
\references{Falissard B, Focused Principal Components Analysis: looking at a correlation matrix with a particular interest in a given variable. Journal of Computational and Graphical Statistics (1999), 8(4): 906-912.}
\author{Bruno Falissard, Bill Morphey}
\examples{
data(sleep)
fpca(sleep,5,c(2:4,7:11)) 
## focused PCA of the duration of paradoxical sleep (dreams, 5th column)
## against constitutional variables in mammals (columns 2, 3, 4, 7, 8, 9, 10, 11).
## Variables inside the red cercle are significantly correlated
## to the dependent variable with p<0.05.
## Green variables are positively correlated to the dependent variable,
## yellow variables are negatively correlated.
## There are three clear clusters of independent variables. 
}
\keyword{multivariate}

