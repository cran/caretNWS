\name{startNWS}
\alias{startNWS}
\title{Start a nws sleigh}
\description{
This function is used to start a nws \code{Sleigh} object
}
\usage{
startNWS(numWorkers = 5, verbose = TRUE)
}
\arguments{
  \item{numWorkers}{the number of compute nodes}
  \item{verbose}{a logical; should a log of the node login process be printed?}
}
\details{
This is a shortcut used for \code{\link{trainNWS}} to start the required sleigh object.
}
\value{
an object of class "sleigh"
}


\seealso{\code{\link{trainNWSControl}}, \code{\link[nws]{sleigh}}}

\keyword{utilities}


