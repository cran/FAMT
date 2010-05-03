\name{annotations}
\alias{annotations}
\docType{data}
\title{Gene annotations data frame
}
\description{A data frame with 6 columns describing the 9893 genes, which expressions are stored in the 'expression' dataset, in terms of functional categories, oligonucleotide size and location on the microarray. See also \code{\link{expression}},\code{\link{covariates}}.
}
\usage{data(annotations)}
\format{
  A data frame with 9893 observations on the following 6 variables.
  \describe{
		\item{\code{ID}}{Gene identification}
    \item{\code{Name}}{Gene annotation (functional categories) (character)}
    \item{\code{Block}}{Location on the microarray(factor)}
    \item{\code{Column}}{Location on the microarray (factor)}
    \item{\code{Row}}{Location on the microarray (factor)}
    \item{\code{Length}}{Oligonucleotide size (numeric vector)}
}
}
%\details{}

\source{UMR Genetique Animale - INRA/AGROCAMPUS OUEST - Rennes, France.}
\references{Blum et al (2010) - A factor model to analyze heterogeneity in gene expression, submitted.

Le Mignon et al.(2010) - Using trancriptome profling to refine QTL regions on chicken chromosome 5. BMC
Genomics, in press.}
\examples{
data(annotations)
dim(annotations) 
summary(annotations)
}
\keyword{datasets}
