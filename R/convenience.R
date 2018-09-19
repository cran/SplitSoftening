auc.trapezoid <- function(predictions, labels) {
  pr <- as.double(predictions)
  lab <- as.integer(labels)-1L
  ndata <- length(labels)
  temp <- .C("auc_trapezoid",
    ndata,
    pr,
    lab,
    result=as.double(0.0),
    PACKAGE="SplitSoftening",
    NAOK=TRUE)
  return(temp$result)
}

#' Create a `soft tree' structure with softening parameters set
#' using one of the named method.

#' @param fit A classification tree - either an object \code{tree} which represents a classification tree,
#' or already a `soft tree' like created by the \code{\link{softsplits}} function.
#' @param ds A data frame used for softening
#' @param method A name of softening method.
#'  One of: "DR0", "DR1", "DR2", ..., "optim_d", "optim_d^2", "optim_d^4", "optim_auc"
#'
#'  The 'method = "DR\emph{x}"' for some number \emph{x}: The softening parameters are set
#'  according to `data ranges' appropriate to tree nodes.
#'  The parameters are configured such that in each node the distance of the boundary of the softened area from split value is
#'  \eqn{2^{-x}r}, where \eqn{r} is the distance from the split value to the furthest data point in the tree node
#'  projected to the direction from the split value to the boundary.
#'
#'  The 'method = "optim_d^\emph{q}"' for some number \emph{q}: The softening parameters are set
#'  by optimization process which minimizes \eqn{\code{mean}((1.0-p)^q)} where \eqn{p} is for each data point in 
#'  \code{ds} the predicted probability of the correct label.
#'
#'  If 'method = "optim_auc"': The classification tree \code{fit} must perform prediction to two classes.
#'  The value of the `area under ROC curve' computed on the data set \code{ds} is maximized by optimization.
#'
#' @return The `soft tree' structure representing the same tree structure
#'  as given in the parameter \code{fit},
#'  but with softening parameters set using the given method.
#'
#' This is a convenience method implemented over \code{\link{softsplits}}
#' and the softening functions from this package.
#'
#' @seealso \code{\link{predictSoftsplits}}.
#' @export
soften <- function( fit, ds, method ) {
  if (inherits(fit, "tree") || inherits(fit, "rpart")) {
    fit <- softsplits(fit)
  }
  compute.softening <- function() {
    if ( ( "DR" == substring(method, 0, 2) ) && !is.na(as.numeric(substring(method, 3))) ) {
      return( softening.by.data.range(fit, ds, 2.0^-as.numeric(substring(method, 3))) )
    }
    if ( "C4.5" == method ) {
      stop( paste("Not implemented method:", method) )
    }
    if ( "optim_" == substring(method, 0, 6) ) {
      yvar.name <- as.character(attr(attr(fit, "terms"), "variables")[-1L][[1]])
      miss <- NULL
      if ( "auc" == substring(method, 7) ) {
        miss <- function(res) {
          return( -auc.trapezoid(res, ds[[yvar.name]] ) )
        }
      } else {
        selector <- matrix(c(1:nrow(ds),as.integer(ds[[yvar.name]])),ncol=2)
        diff.vec <- function( res ) {
          return( 1.0 - res[selector] )
        }
        if ( "d" == substring(method, 7) ) {
          miss <- function( res ) {
            return ( mean( diff.vec( res ) ) )
          }
        } else if ( "d^" == substring(method, 7, 8) ) {
          exponent <- as.numeric(substring(method, 9))
          if ( is.na(exponent) ) {
            return( NULL )
          }
          miss <- function( res ) {
            return ( mean( diff.vec( res )^exponent ) )
          }
        }
      }
      if ( !is.null(miss) ) {
        return( softening.optimized( fit, ds, miss, verbosity=5 ) )
      }
    }
    return( NULL )
  }

  softening <- compute.softening()

  if ( is.null(softening) ) {
    stop( paste("Unknown softening method:", method) )
  }
  return( softening )
}

