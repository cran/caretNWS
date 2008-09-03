trainNWSControl <- function(
                            method = "boot",
                            number = ifelse(method == "cv", 10, 25),
                            verboseIter = TRUE,
                            returnData = TRUE,
                            returnResamp = "final",
                            p = .75,
                            summaryFunction = caret:::defaultSummary,
                            selectionFunction = "best",
                            index = NULL,
                            start = startNWS)
{
  if(is.null(selectionFunction)) stop("null selectionFunction values not allowed")
  if(!(returnResamp %in% c("all", "final", "none"))) stop("incorrect value of returnResamp")
  list(
       method = method,
       number = number,
       verboseIter = verboseIter,
       returnData = returnData,
       returnResamp = returnResamp,
       p = p,
       summaryFunction = summaryFunction,
       selectionFunction = selectionFunction,
       index = index,
       start = startNWS)
}
