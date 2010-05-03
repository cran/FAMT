\name{expression}
\alias{expression}
\docType{data}
\title{Gene expressions data frame
}
\description{This dataset concerns hepatic transciptome profiles of 43 half sib male chickens selected for their variability on abdominal fatness (AF). Genes are in rows (9893 genes) and arrays in columns (43 arrays).}

\usage{data(expression)}
\format{
  A data frame with 9893 genes on 43 arrays. 
  %\describe{}
}
%\details{


\source{UMR Genetique Animale - INRA/AGROCAMPUS OUEST - Rennes, France.}
\references{Blum et al (2010) - A factor model to analyze heterogeneity in gene expression, submitted.

Le Mignon et al.(2010) - Using trancriptome profling to refine QTL regions on chicken chromosome 5. BMC
Genomics, in press.}

\examples{
data(expression)
dim(expression)
summary(expression)
}
\keyword{datasets}
