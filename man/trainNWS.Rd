\name{trainNWS}
\alias{trainNWS}
\title{Fit Predictive Models over Different Tuning Parameters}
\description{
  This function sets up a grid of tuning parameters for a number of classification and regression routines,
  fits each model and calculates a resampling based performance measure.
}
\usage{
trainNWS(x, y, method = "rf",  ...,
   metric = ifelse(is.factor(y), "Accuracy", "RMSE"),
      maximize = ifelse(metric == "RMSE", FALSE, TRUE),
   sleighObj = NULL,
   trControl = trainNWSControl(), 
   tuneGrid = NULL,
   tuneLength = 3)
}
\arguments{
  \item{x}{a data frame containing training data where samples are in rows and features are in columns.}
  \item{y}{a numeric or factor vector containing the outcome for each sample.}
  \item{method}{a string specifying which classification or regression model to use. Possible values are: \code{ada}, \code{bagEarth}, \code{bagFDA}, \code{blackboost}, \code{cforest}, \code{ctree}, \code{ctree2}, \code{earth}, \code{enet}, \code{fda}, \code{gamboost}, \code{gaussprPoly}, \code{gaussprRadial}, \code{gbm}, \code{glm}, \code{glmboost}, \code{gpls}, \code{J48}, \code{JRip}, \code{knn}, \code{lasso}, \code{lda}, \code{lm}, \code{lmStepAIC}, \code{LMT}, \code{logitBoost}, \code{lssvmPoly}, \code{lssvmRadial}, \code{lvq}, \code{M5Rules}, \code{mda}, \code{multinom}, \code{nb}, \code{nnet}, \code{pam}, \code{pcaNNet}, \code{pda},  \code{pda2},\code{penalized}, \code{pls}, \code{ppr}, \code{qda}, \code{rda}, \code{rf}, \code{rpart}, \code{rvmPoly}, \code{rvmRadial}, \code{sda}, \code{sddaLDA}, \code{sddaQDA}, \code{slda}, \code{sparseLDA}, \code{spls}, \code{splsda}, \code{superpc}, \code{svmPoly}, \code{svmRadial} and \code{treebag}. See the Details section below.}  
  \item{\ldots}{ arguments passed to the classification or regression routine (such as \code{\link[randomForest]{randomForest}}). Errors will occur if values
         for tuning parameters are passed here.}
  \item{metric}{a string that specifies what summary metric will be used to select the optimal model. Possible values are "RMSE" and "Rsquared" for
     regression and "Accuracy" and "Kappa" for classification.(NOTE: If given, this argument must be named.)}
  \item{maximize}{a logical: should the metric be maximized or minimized?}
  \item{sleighObj}{a \code{\link[nws]{sleigh}} object for an existing nws session. This object is not destroyed after \code{trainNWS} closes.}
  \item{trControl}{a list of values that define how this function acts. See \code{\link{trainNWSControl}}. (NOTE: If given, this argument must be named.)}
  \item{tuneGrid}{a data frame with possible tuning values. The columns are named the same as the tuning parameters in each
         method preceded by a period (e.g. .decay, .lambda). See the function \code{\link[caret]{createGrid}} in this package for more details.
         (NOTE: If given, this argument must be named.)}
  \item{tuneLength}{an integer denoting the number of levels for each tuning parameters that should be
         generated by \code{\link[caret]{createGrid}}. (NOTE: If given, this argument must be named.)}
}
\details{
\code{trainNWS} can be used to tune models by picking the complexity parameters that are associated with the optimal resampling statistics. For particular model, a grid of parameters (if any) is created and the model is trained on slightly different data for each candidate combination of tuning parameters. Across each data set, the performance of held-out samples is calculated and the mean and standard deviation is summarized for each combination. The combination with the optimal resampling statistic is chosen as the final model and the entire training set is used to fit a final model.

Currently, the \code{trainNWS} function does not support model specification via a formula. It assumes that all of the predictors are numeric (perhaps generated by \code{\link[stats]{model.matrix}}).

A variety of models are currently available. The table below enumerates the models and the values of the \code{method} argument, as well as the complexity parameters used by \code{trainNWS}.

\tabular{lccc}{
\bold{Model} \tab \bold{\code{method} Value} \tab \bold{Package} \tab \bold{Tuning Parameter(s)} \cr
Generalized linear model \tab \code{glm} \tab \pkg{stats} \tab none \cr
Recursive partitioning \tab \code{rpart} \tab \pkg{rpart} \tab \code{maxdepth} \cr  
\tab \code{ctree} \tab \pkg{party} \tab \code{mincriterion} \cr   
\tab \code{ctree2} \tab \pkg{party} \tab \code{maxdepth} \cr 
Boosted trees \tab \code{gbm} \tab \pkg{gbm} \tab \code{interaction depth}, \cr
\tab \tab \tab \code{n.trees}, \code{shrinkage}  \cr
\tab \code{blackboost} \tab \pkg{mboost} \tab \code{maxdepth}, \code{mstop}\cr
\tab \code{ada} \tab \pkg{ada} \tab \code{maxdepth}, \code{iter}, \code{nu}\cr            
Boosted regression models \tab \code{glmboost} \tab \pkg{mboost} \tab \code{mstop}\cr   
\tab \code{gamboost} \tab \pkg{mboost} \tab  \code{mstop}\cr    
\tab \code{logitboost} \tab \pkg{caTools} \tab  \code{nIter}\cr    
Random forests \tab  \code{rf} \tab \pkg{randomForest} \tab \code{mtry} \cr
\tab \code{cforest} \tab \pkg{party} \tab \code{mtry} \cr   
Bagged trees \tab \code{treebag} \tab \pkg{ipred} \tab None \cr 
Neural networks \tab  \code{nnet} \tab \pkg{nnet} \tab \code{decay}, \code{size} \cr
Projection pursuit regression \tab \code{ppr} \tab \pkg{stats} \tab \code{nterms} \cr
Partial least squares \tab \code{pls} \tab \pkg{pls}, \pkg{caret} \tab \code{ncomp} \cr
Sparse partial least squares \tab \code{spls} \tab \pkg{spls}, \pkg{spls} \tab \code{K}, \code{eta} \cr
Support vector machines (RBF)  \tab \code{svmradial} \tab \pkg{kernlab} \tab \code{sigma}, \code{C}\cr 
Support vector machines (polynomial)  \tab \code{svmpoly} \tab \pkg{kernlab} \tab \code{scale}, \code{degree}, \code{C}\cr 
Relevance vector machines (RBF)  \tab \code{rvmradial} \tab \pkg{kernlab} \tab \code{sigma}\cr 
Relevance vector machines (polynomial)  \tab \code{rvmpoly} \tab \pkg{kernlab} \tab \code{scale}, \code{degree}\cr 
Least squares support vector machines (RBF)  \tab \code{lssvmradial} \tab \pkg{kernlab} \tab \code{sigma}\cr 
Gaussian processes (RBF)  \tab \code{guassprRadial} \tab \pkg{kernlab} \tab \code{sigma}\cr 
Gaussian processes (polynomial)  \tab \code{guassprPoly} \tab \pkg{kernlab} \tab \code{scale}, \code{degree}\cr 
Linear least squares \tab \code{lm} \tab \pkg{stats} \tab None \cr 
Multivariate adaptive regression splines \tab \code{earth} \tab \pkg{earth} \tab \code{degree}, \code{nprune} \cr
Bagged MARS \tab \code{bagEarth} \tab \pkg{caret},  \pkg{earth}\tab \code{degree}, \code{nprune} \cr 
M5 rules \tab \code{M5Rules} \tab \pkg{RWeka} \tab \code{pruned} \cr 
Elastic net \tab \code{enet} \tab \pkg{elasticnet} \tab \code{lambda}, \code{fraction} \cr
The Lasso \tab \code{enet} \tab \pkg{elasticnet} \tab \code{fraction} \cr
Penalized linear models \tab \code{penalized} \tab \pkg{penalized} \tab \code{lambda1}, \code{lambda2}\cr
Supervised principal components \tab \code{superpc} \tab \pkg{superpc} \tab \code{n.components}, \code{threshold} \cr 
Linear discriminant analysis \tab \code{lda} \tab \pkg{MASS} \tab  None\cr 
Quadratic discriminant analysis \tab \code{qda} \tab \pkg{MASS} \tab  None\cr 
Stabilised Linear discriminant analysis \tab \code{slda} \tab \pkg{ipred} \tab  None\cr 
Stepwise diagonal discriminant analysis \tab \code{sddaLDA}, \code{sddaQDA} \tab \pkg{SDDA} \tab None \cr
Shrinkage discriminant analysis \tab \code{sda} \tab \pkg{sda} \tab diagonal \cr
Regularized discriminant analysis \tab \code{rda} \tab \pkg{klaR} \tab \code{lambda}, \code{gamma} \cr
Mixture discriminant analysis \tab \code{mda} \tab \pkg{mda} \tab \code{subclasses} \cr
Penalized discriminant analysis \tab \code{pda} \tab \pkg{mda} \tab \code{lambda}\cr
\tab \code{pda2} \tab \pkg{mda} \tab \code{df}\cr
Stabilised linear discriminant analysis \tab \code{slda} \tab \pkg{ipred} \tab None\cr
Flexible discriminant analysis (MARS) \tab \code{fda} \tab \pkg{mda}, \pkg{earth} \tab \code{degree}, \code{nprune} \cr
Bagged FDA \tab \code{bagFDA} \tab \pkg{caret},  \pkg{earth} \tab \code{degree}, \code{nprune} \cr  
Logistic/multinomial regression \tab \code{multinom} \tab \pkg{nnet} \tab \code{decay}\cr 
C4.5 decision trees \tab \code{J48} \tab \pkg{RWeka} \tab \code{C}\cr
k nearest neighbors \tab \code{knn3} \tab \pkg{caret} \tab \code{k} \cr 
Nearest shrunken centroids \tab \code{pam} \tab \pkg{pamr} \tab \code{threshold} \cr  
Naive Bayes \tab \code{nb} \tab \pkg{klaR} \tab \code{usekernel} \cr
Generalized partial least squares\tab \code{gpls} \tab \pkg{gpls} \tab \code{K.prov} \cr
Learned vector quantization \tab \code{lvq} \tab \pkg{class} \tab  \code{k} 
}


By default, the function \code{\link[caret]{createGrid}} is used to define the candidate values of the tuning parameters. The user can also specify their own. To do this, a data fame is created with columns for each tuning parameter in the model. The column names must be the same as those listed in the table above with a leading dot. For example, \code{ncomp} would have the column heading \code{.ncomp}. This data frame can then be passed to \code{\link[caret]{createGrid}}.

In some cases, models may require control arguments. These can be passed via the three dots argument. Note that some models can specify tuning parameters in the control objects. If specified, these values will be superseded by those given in the \code{\link[caret]{createGrid}} argument.

The vignette entitled "caret Manual -- Model Building" in the caret
package has more details and examples related to this function.

\code{caretNWS} is a parallel version of the \code{\link[caret]{train}} function in the \pkg{caret} package. This function implements a combination of sequential and parallel jobs. For example, if a PLS model with 10 candidate values for the number of retained components is requested along with 10-fold cross--validation, the 10 jobs for each caudate tuning parameter are submitted in parallel. Once finished, the process is started again with the second candidate tuning parameter etc.

The function also has a fault-tolerance function where any jobs that are abnormally long (say 20x the longest job to date) can be killed automatically. In this way, a problem with a worker node may not destroy the results to date. For example, if the models are tuned using 25 bootstrap results, the failure of one node will cause the function to tune the model with the remainder and keep going.

The \code{nwsStart} can be dependent on the configuration of the user's system. The example function given in the package may not work out of the box.

}



\value{
  A list is returned of class \code{train} containing:
  \item{modelType }{an identifier of the model type.}
  \item{results }{a data frame the training error rate and values of the tuning parameters.}
  \item{call }{the (matched) function call with dots expanded}
  \item{dots}{a list containing any ... values passed to the original call}
  \item{metric}{a string that specifies what summary metric will be used to select the optimal model.}
  \item{trControl}{the list of control parameters.}
   \item{finalModel}{an fit object using the best parameters}
   \item{trainingData}{a data frame}
   \item{resample}{A data frame with columns for each performance
     metric. Each row corresponds to each resample. If leave-one-out
     cross-validation or out-of-bag estimation methods are requested,
     this will be \code{NULL}. The \code{returnResamp} argument of \code{\link{trainNWSControl}}}
  \item{perfNames}{a character vector of performance metrics that are produced by the summary function}
  \item{maximize}{a logical recycled  from the function arguments.}
}

\author{Max Kuhn}

\references{Kuhn (2008), ``Building Predictive Models in R Using the caret'' (\url{http://www.jstatsoft.org/v28/i05/})}

\seealso{\code{\link[caret]{train}}, \code{\link[caret]{createGrid}}, \code{\link[caret]{createFolds}}}
\examples{
\dontrun{
data(iris)
TrainData <- iris[,1:4]
TrainClasses <- iris[,5]

knnFit <- trainNWS(TrainData, TrainClasses, "knn", tuneLength = 10,
   trControl = trainNWSControl(method = "boot"))
}
}

\keyword{models}
