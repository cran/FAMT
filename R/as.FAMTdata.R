as.FAMTdata <-
function (expression, covariates, annotations = NULL, idcovar = 1, idannot = NULL) {
   expr = expression
   covar = covariates
   annot = data.frame(ID = rownames(expr))
   if (!is.null(annotations)) {
       annot = annotations
       if (is.null(idannot)) {
           if (!any(is.element(colnames(annot), "ID")))
               stop("One of the columns in annotations should be named ID")
       }
       if (!is.null(idannot)) {
           if (!any(is.element(1:ncol(annot), idannot)))
               stop(paste("idannot should be in 1:", ncol(annot),
                 sep = ""))
           names(annot)[idannot] = "ID"
       }
   }
   if (ncol(expr) != nrow(covar))
       stop("Dimensions of expression and covariates should correspond.")
   if (!any(is.element(1:ncol(covar), idcovar)))
       stop(paste("idcovar should be in 1:", ncol(covar), sep = ""))
   if ((!is.null(annotations)) & (nrow(expr) != nrow(annot)))
       stop("Numbers of rows of expression and annotations should correspond.")
   if (!setequal(colnames(expr),as.character(covar[,idcovar])))
       stop("Names should correspond in expression and covariates")
   ordcovar = order(colnames(expr))
   res = list(expression = expr[,ordcovar], covariates = covar[ordcovar,], annotations = annot,
       idcovar = idcovar)
   class(res) = list("FAMTdata", "list")
   return(res)
} 
