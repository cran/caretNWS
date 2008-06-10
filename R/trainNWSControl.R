trainNWSControl <- function(
                            method = "boot",
                            number = ifelse(method == "cv", 10, 25),
                            verboseIter = TRUE,
                            returnData = TRUE,
                            p = .5,
                            selectionFunction = "best",
                            index = NULL,
                            start = startNWS)
{
  list(
       method = method,
       number = number,
       verboseIter = verboseIter,
       returnData = returnData,
       p = p,
       selectionFunction = selectionFunction,
       index = index,
       start = start
       )
}

