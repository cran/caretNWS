\name{startNWS}
\alias{startNWS}
\alias{makeSleighStarter}
\title{Functions for starting Sleigh objects}
\description{
 These functions can be used to quickly create Sleigh objects.
}
\usage{
startNWS()
makeSleighStarter(workerCount = 5, verbose = TRUE, 
    launch = "local",
    timeout = 300,
    nodeList = rep("localhost", workerCount), ...)
}
\arguments{
  \item{workerCount}{the number of workers (i.e. compute nodes)}
  \item{verbose}{a logical: should the details of acquiring nodes be shown? }
  \item{launch}{a character string for the method of starting jobs on compute nodes. This is specific to the configuration of your grid/cluster.  See \code{\link[nws]{sleigh}} for details and examples. Examples are: "local", "ssh" or "bsub". See the \pkg{nws} package documentation for more details.}
  \item{timeout}{The number of seconds to wait while compute nodes are acquired. See \code{\link[nws]{status}} for details and examples.}
  \item{nodeList}{a list of hosts that workers will be created. Note that this argument is ignored when launch equals "local". The default value is c('localhost', 'localhost', 'localhost')}
  \item{\dots}{Optional arguments to pass to \code{\link[nws]{sleigh}}.}
}
\details{
By default, \code{\link{trainNWS}} uses these functions to create Sleigh objects (right now, you cannot pass Sleigh objects to the train function).

These fucntions are specified using the  \code{\link{trainNWSControl}} function.
}
\value{
A function is produces that will create sleigh objects. This will then be called from within \code{\link{trainNWS}}.
}
\author{Steve Weston, Nathan Coulter}

\seealso{ \code{\link{trainNWS}}, \code{\link{trainNWSControl}}, \code{\link[nws]{sleigh}}, \code{\link[nws]{status}}}

\keyword{utilities}
