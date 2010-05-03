\name{FAMT-package}
\alias{FAMT-package}
\alias{FAMT}
\docType{package}
\title{Factor Analysis for Multiple Testing (FAMT) : simultaneous tests under dependence in high-dimensional data
}
\description{
The method proposed in this package takes into account the impact of dependence on multiple testing procedures for high-throughput data as proposed by Friguet et al. (2009). The common information shared by all the variables is modeled by a factor analysis structure. The number of factors considered in the model is chosen to reduce the variance of the number of false discoveries. The model parameters are estimated thanks to an EM algorithm. Factor-adjusted tests statistics are derived, as well as the associated p-values. The proportion of true null hypotheses (an important parameter when controlling the false discovery rate) is also estimated from the FAMT model. Diagnostic plots are proposed to interpret and describe the factors.
}
\details{
\tabular{ll}{
Package: \tab FAMT\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2010-05-03\cr
License: \tab GPL \cr
LazyLoad: \tab yes\cr
}

The \code{\link{as.FAMTdata}} function creates a single R object containing the data stored in two mandatory data-frames: 

- the 'expression' dataset with m rows (if m tests) and n columns (n is the sample size) containing the observations of the responses.

- the 'covariates' dataset with n rows and at least 2 columns, one giving the specification to match 'expression' and 'covariates' and the other one containing the observations of at least one covariate. 

An optional dataset can be provided to help interpreting the factors: 'annotations' with m rows and at least one column to identify the variables (ID).

The whole multiple testing procedure is provided in a single function, \code{\link{modelFAMT}}, but you can also choose to apply the procedure step by step, using the functions : 

\code{\link{nbfactors}} (Estimation of the optimal number of factors)
\code{\link{emfa}} (EM fitting of the Factor Analysis model).

A function \code{\link{summaryFAMT}} provides some key elements of classical summaries either on 'FAMTdata' or 'FAMTmodel'.

The estimation of the proportion of true null hypotheses from a 'FAMTmodel' is done by the function \code{\link{pi0FAMT}}. 

The \code{\link{defacto}} function provides diagnostic plots to interpret and describe the factors.

The \code{\link{raw.pvalues}} function calculates the individual test statistics and corresponding p-values.
}

\author{
David Causeur, Chloe Friguet, Magalie Houee-Bigot, Maela Kloareg.

Maintainer: \email{David.Causeur@agrocampus-ouest.fr}
}

\references{
Friguet C., Kloareg M. and Causeur D. (2009). A factor model approach to multiple testing under dependence. Journal of the American Statistical Association, 104:488, p.1406-1415

\url{http://famt.free.fr/}
}
\keyword{Factor analysis, Multiple testing, False discovery rate, Dependence.}

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

# Calculation of classical p-values
############################################
# test on the 6th covariate (not run) : 
# rawpval = raw.pvalues(chicken,x=6)
# hist(rawpval$pval)

# with a supplementary covariate (third column of the covariates data frame)
rawpval = raw.pvalues(chicken,x=c(3,6),test=6)
hist(rawpval$pval)

# FAMT complete multiple testing procedure
############################################
model = modelFAMT(chicken,x=c(3,6),test=6)
# summary on the 'FAMT model'
summaryFAMT(model)

# Estimation of the Proportion of True Null Hypotheses (not run)
############################################
# pi0FAMT(model,method="density",diagnostic.plot=TRUE)

# Factors description
############################################
chicken.defacto = defacto(model,axes=1:2,select.covar=4:5,select.annot=3:6,
cex=0.6)

# Other useful functions : the FAMT procedure step by step (not run)
############################################

# number of factors in the FA model
# nbfactors(chicken,x=c(3,6),test=6)

# Fitting the FA model
# chicken.emfa = emfa(chicken,nbf=3,x=c(3,6),test=6)
# chicken.emfa$commonvar
}
