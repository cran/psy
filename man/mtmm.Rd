\name{mtmm}
\alias{mtmm}
\title{Multitrait Multimethod approach of scale validation}
\description{
 This function is destinated to assess the convergent and discriminant validity of subscales of a given scale.
 Items belonging to the same subscale should correlate highly amongst themselves.
 Items belonging to different subscales should not correlate highly.
 This approach is simpler and more robust than confirmatory factor analysis (CFA).
 It can be interesting to verify (at least approximately) the proposed structure of an existing instrument in a new population.
 Most psychometricians will however prefer CFA.
 }
\usage{mtmm(datafile,x,color=FALSE,itemTot=FALSE,graphItem=FALSE,stripChart=FALSE,namesDim=NULL)
 }
\arguments{
  \item{datafile}{name of datafile}
  \item{x}{a list of variable names (as many elements as there are subscales)}
  \item{color}{boxplot are in colour: FALSE = colourless just in grey and white (by default), TRUE = with colours}
  \item{itemTot}{if TRUE, for subscale i (i=1,...,n), boxplot of Pearson's correlations between total score of subscale i and the items of subscale j (j=1,...n).
  If j=i, the item is omited in the computation of the total score. If FALSE, for subscale i (i=1,...,n), boxplot of Pearson's correlations between the items of
  subscale i and the items of subscale j (j=1,...n)}
  \item{graphItem}{if TRUE represents graphically each correlation}
  \item{stripChart}{if TRUE, dot charts are preferred to boxplots. Used with small number of items}
  \item{namesDim}{Labels foreach boxplots}
}

\value{
  For subscale i (i=1,...,n), displays the n boxplots of the distributions of the Pearson's correlations between items of subscale i and items of subscale j (j=1,...,n).
  If j=i, the correlation of a given item with itself is ommited.
  Boxplot for i=j (grey by default) should be above boxplots for i!=j.
  Likewise, the correlation of an item with the global score of its subscale should be above its correlations with the global score of the other subscales.
}
\author{Adeline Abbe}

\examples{

data(ehd)

par(mfrow=c(1,5))
mtmm(ehd,list(c("e15","e18","e19","e20"),c("e4","e5","e6","e14","e17"),c("e11","e13","e16")
,c("e1","e10","e12"),c("e2","e3","e7","e8","e9")))

# Boxplots of the distributions of the Pearson's correlations between total score of
# subscale i and the items of subscale j
par(mfrow=c(1,5))
mtmm(ehd,list(c("e15","e18","e19","e20"),c("e4","e5","e6","e14","e17"),c("e11","e13","e16")
,c("e1","e10","e12"),c("e2","e3","e7","e8","e9")))

# Pearson's correlations between total score of subscale i and all items
par(mfrow=c(3,2))
mtmm(ehd,list(c("e15","e18","e19","e20"),c("e4","e5","e6","e14","e17"),c("e11","e13","e16")
,c("e1","e10","e12"),c("e2","e3","e7","e8","e9")),graphItem=TRUE)

}

