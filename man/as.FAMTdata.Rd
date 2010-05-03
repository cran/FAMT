\name{as.FAMTdata}
\alias{as.FAMTdata}
\title{Create a FAMT data object from an expression, covariates and annotations dataset}

\description{The function creates a FAMT data object containing the expression and the covariates datasets, and the annotations dataset if provided.}

\usage{
as.FAMTdata(expression, covariates, annotations = NULL, idcovar = 1, 
idannot = NULL)
}
\arguments{
  \item{expression}{An expression data frame with genes in rows and arrays in columns. The arrays are identified by the column names.}
  \item{covariates}{A covariate data frame with arrays in rows, and covariates in columns. One column must contain the array identification.}
  \item{annotations}{An optional data frame containing informations on the genes (NULL by default)}
  \item{idcovar}{The column number corresponding to the array identification in the covariate data frame (1 by default)}
  \item{idannot}{The column number corresponding to the gene identification in annotation data frame (NULL by default)}
}
\details{The \code{\link{as.FAMTdata}} function creates a single R object containing the data stored in two mandatory data-frames: 
- the 'expression' dataset with m rows (if m tests) and n columns (n is the sample size) containing the observations of the responses.
- the 'covariates' dataset with n rows and at least 2 columns, one giving the specification to match 'expression' and 'covariates' and the other one containing the observations of at least one covariate. 
An optional dataset can be provided to help interpreting the factors: 'annotations' with m rows and at least one column to identify the variables (ID).
}
\value{
	\item{expression }{The expression data frame}
  \item{covariates }{The covariate data frame}
	\item{annotations}{The optional data frame containing annotations. The genes annotations such as the functional categories should be in a character form, not in a factor form.}
	\item{idcovar}{The column number corresponding to the array identification in the covariate data frame (which should correspond to the column names in 'expression')}
}
%\references{}

\author{ David Causeur }

\note{The class of the data imported with "as.FAMTdata" is called 'FAMTdata'. We advise to carry out a summary of FAMT data with the function \code{\link{summaryFAMT}}.}

\seealso{\code{\link{summaryFAMT}}}

\examples{
# The data are divided into two mandatory data-frames: the gene expressions, 
#  and the covariates, and an optional dataset, annotations.

# The expression dataset with 9893 rows (genes) and 43 columns (arrays)
#  containing the observations of the responses.
# The covariates dataset with 43 rows (arrays) and 6 columns: 
#  the second column gives the specification to match 'expression' 
#  and 'covariates' (array identification), the other ones contain
#  the observations of covariates.
# The optional dataset 'annotations' contains 9893 rows (genes) and 
#  6 columns to help interpreting the factors, the first one (ID) 
#  identifies the variables (genes). 

data(expression)
data(covariates)
data(annotations)

# Create the 'FAMTdata'
############################################
chicken = as.FAMTdata(expression,covariates,annotations,idcovar=2)
# 'FAMTdata' summary
summaryFAMT(chicken)

}
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
