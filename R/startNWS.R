makeSleighStarter <-
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

startNWS <- makeSleighStarter()

