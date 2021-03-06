% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/soft.R
\name{softsplits}
\alias{softsplits}
\title{Create `soft tree' structure from a tree object.}
\usage{
softsplits(fit)
}
\arguments{
\item{fit}{A tree object: must be a classification tree}
}
\value{
A data structure suitable for softening splits in the tree
 and for evaluation of `soft tree' on submitted data.
 The returned object is ready for softening, but it is not yet softened.
 The result of prediction for some data with the returned object
     is still the same as with the original tree \code{fit}.
}
\description{
Create `soft tree' structure from a tree object.
}
\seealso{
\code{\link{predictSoftsplits}}.
}
