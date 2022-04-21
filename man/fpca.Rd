\name{fpca}
\alias{fpca}
\title{Focused Principal Components Analysis}
\description{
Graphical representation similar to a principal components analysis but adapted to data structured with dependent/independent variables
}
\usage{
fpca(formula=NULL,y=NULL, x=NULL, data, cx=0.75, pvalues="No",
     partial="Yes", input="data", contraction="No", sample.size=1)
}
\arguments{
  \item{formula}{"model" formula, of the form y ~ x }
  \item{y}{column number of the dependent variable}
  \item{x}{column numbers of the independent (explanatory) variables}
  \item{data}{name of datafile}
  \item{cx}{size of the lettering (0.75 by default, 1 for bigger letters, 0.5 for smaller)}
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
A plot (q plots in fact).
}
\references{Falissard B, Focused Principal Components Analysis: looking at a correlation matrix with a particular interest in a given variable. Journal of Computational and Graphical Statistics (1999), 8(4): 906-912.}
\author{Bruno Falissard, Bill Morphey, Adeline Abbe}
\examples{
data(sleep)
fpca(Paradoxical.sleep~Body.weight+Brain.weight+Slow.wave.sleep+Maximum.life.span+
     Gestation.time+Predation+Sleep.exposure+Danger,data=sleep)
fpca(y="Paradoxical.sleep",x=c("Body.weight","Brain.weight","Slow.wave.sleep",
     "Maximum.life.span","Gestation.time","Predation","Sleep.exposure","Danger"),data=sleep)


## focused PCA of the duration of paradoxical sleep (dreams, 5th column)
## against constitutional variables in mammals (columns 2, 3, 4, 7, 8, 9, 10, 11).
## Variables inside the red cercle are significantly correlated
## to the dependent variable with p<0.05.
## Green variables are positively correlated to the dependent variable,
## yellow variables are negatively correlated.
## There are three clear clusters of independent variables.

corsleep <- as.data.frame(cor(sleep[,2:11],use="pairwise.complete.obs"))
fpca(Paradoxical.sleep~Body.weight+Brain.weight+Slow.wave.sleep+Maximum.life.span+
    Gestation.time+Predation+Sleep.exposure+Danger,
data=corsleep,input="Cor",sample.size=60)

## when missing data are numerous, the representation of a pairwise correlation
## matrix may be preferred (even if mathematical properties are not so good...)

numer <- c(2:4,7:11)
l <- length(numer)
resu <- vector(length=l)
for(i in 1:l)
{
int <- sleep[,numer[i]]
mod <- lm(sleep$Paradoxical.sleep~int)
resu[i] <-  summary(mod)[[4]][2,4]*sign(summary(mod)[[4]][2,1])
}
fpca(Paradoxical.sleep~Body.weight+Brain.weight+Slow.wave.sleep+Maximum.life.span+
     Gestation.time+Predation+Sleep.exposure+Danger,
data=sleep,pvalues=resu)
## A representation with p values
## When input="Cor" or pvalues="Yes" partial is turned to "No"

mod <- lm(sleep$Paradoxical.sleep~sleep$Body.weight+sleep$Brain.weight+
sleep$Slow.wave.sleep+sleep$Maximum.life.span+sleep$Gestation.time+
sleep$Predation+sleep$Sleep.exposure+sleep$Danger)
resu <-  summary(mod)[[4]][2:9,4]*sign(summary(mod)[[4]][2:9,1])
fpca(Paradoxical.sleep~Body.weight+Brain.weight+Slow.wave.sleep+Maximum.life.span+
     Gestation.time+Predation+Sleep.exposure+Danger,
data=sleep,pvalues=resu)
## A representation with p values which come from a multiple linear model
## (here results are difficult to interpret)
}
\keyword{multivariate}

