makeStartNWS <-
function(workerCount = 5, verbose = TRUE, launch = "local",
    timeout = 300,
    nodeList = rep("localhost", workerCount), ...)
{
    function()
    {
       if (identical(launch, "bsub"))
       {
          launch <- function(host, options) c("bsub")
       }
       s <- sleigh(workerCount = workerCount, verbose = verbose,
                 launch = launch, nodeList = nodeList, ...)
       stat <- status(s, timeout = timeout, closeGroup = TRUE)
       if (stat$numWorkers > 1)
       {
          if(verbose) cat("Number of workers:", stat$numWorkers, "\n")
       } else {
          if(verbose) cat("Number of workers:", stat$numWorkers, "\n")
          stopSleigh(s)
          stop("error creating sleigh")
       }

       s
    }
}

startNWS <- makeStartNWS()

                                        

#startNWS <- function(numWorkers = 5, verbose = TRUE)
#{
#   s <- sleighPro(
#      launch = function(user, host, options) c('bsub'), 
#      nwsHost="glnx011",
#      nodeList = as.character(1:numWorkers),
#      verbose = verbose)
#
#    # Wait up to 5 minutes for the workers to join
#    stat <- status(s, timeout = 5 * 60, closeGroup = TRUE)
#    if (stat$numWorkers > 1)
#    {
#       if(verbose) cat("Number of workers:", stat$numWorkers, "\n")
#    } else {
#       if(verbose) cat("Number of workers:", stat$numWorkers, "\n")
#       stopSleigh(s)
#       stop("error creating sleigh")
#    }
#
#   s
#}
