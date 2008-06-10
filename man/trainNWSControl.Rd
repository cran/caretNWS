\name{trainNWSControl}
\alias{trainNWSControl}
\title{Control parameters for train}
\description{Control of printing and resampling for \code{\link{trainNWS}}}
\usage{
trainNWSControl(
                method = "boot",
                number = ifelse(method == "cv", 10, 25),
                verboseIter = TRUE,
                returnData = TRUE,
                p = 0.5,
                selectionFunction = "best",
                index = NULL,
                start = startNWS) 
}
\arguments{
  \item{method}{The resampling method: \code{boot}, \code{cv},
    \code{LOOCV},  \code{LGOCV} (for repeated training/test splits), or
    \code{oob} (only for random forest, bagged trees, bagged earth, bagged flexible discriminant analysis, or conditional tree forest models)}
  \item{number}{Either the number of folds or number of resampling iterations}
  \item{verboseIter}{A logical for printing a training log.}
  \item{returnData}{A logical for saving the data}
  \item{p}{For leave-group out cross-validation: the training percentage}
  \item{selectionFunction}{the function used to select the optimal tuning parameter. This can be a name of the function or the funciton itself. See \code{\link[caret]{best}} for details and other options.}
  \item{index}{a list with elements for each resampling iteration. Each
    list element is the sample rows used for training at that
    iteration.}
  \item{start}{a function that starts a \code{\link[nws]{slighPro}}
    object. Note: if the \code{sleighObj} is not NULL in the \code{link{trainNWS}} call,
    this function is ignored. See the \code{link{startNWS}} function in this package as an
    example}
}

\value{
 An echo of the parameters specified
}
\author{max Kuhn}

\keyword{utilities}
