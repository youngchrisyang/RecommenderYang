#include FNN

slice <- function(x, n) split(x, as.integer((seq_along(x) - 1) / n))



#' Recommendations base function
#'
#' @param data A binaryRatingMatrix.
#' @param alg The algorithm we will use to get recommendations.
#' @param n The number of recommendations we want for each user.
#' @param parameter Parameters specific to the algorithm we will use.
#' @param progress if TRUE the time usage for each step will be printed.
#' @param keepModel If TRUE the function will return the computed model.
#' @param oldmodel A previously computed model to be used for predicitons.
#' @param modelsize The maximum size for the model.
#' @param testsize The maximum size for the test set.
#' @param partsize The size for prediction partitions.
#' @param reduction If TRUE we will use dimensionality reduction on the train set.
#' @param eval.alg If TRUE the function will return the evaluation confusion matrix
#'
#' @return A three element list:
#'  [[1]] contains the recommendations;
#'  [[2]] contains the model if \code{keepModel} is TRUE;
#'  [[3]] contains a confusion matrix if \code{eval.alg} is TRUE.
#' @examples
#' recommend(data=ratings.sample,alg="POPULAR",modelsize=20000,testsize=1000,partsize=10000,eval.alg=T,n=5,keepModel=T)
#'
#' @export
recommend <- function(data,alg, n=1, parameter = NULL,
                     progress=F, keepModel=F,oldmodel=NULL,
                     modelsize = 40000, testsize = 1000, partsize=10000,
                     reduction=F,eval.alg=F,
                     newdata=NULL) {


  model <- NULL
  dusers <- nrow(data)
  ditems <- ncol(data)
  time_usage <- function(x,y) cat(x,": ",y[1]+y[2]," seconds\n",sep="")
  time_reduce <- 0
  time_model <- 0
  time_predict <- 0
  time_evaluate <- 0


  train.set <- data[sample(1:dusers, dusers*0.9), ]
  if(packageDescription("recommenderlab")$Version<"0.1-8"){
    train.users <- colnames(train.set@data@data)
    all.users <- colnames(data@data@data)
  }else{
    train.users <- train.set@data@itemsetInfo$itemsetID
    all.users <- data@data@itemsetInfo$itemsetID
  }
  match.indexes <- all.users %in% train.users
  test.set <- data[!match.indexes&(rowCounts(data)>2),]

  if(reduction){
    #time_reduce <- system.time(
    #reduce data
    nv <- min(dusers,modelsize)
    red <- svd(Matrix::t(data@data@data),nu=0,nv=nv)
    #print(dim(red$v))
    #red <- as(biclust::binarize(abs(t(red$v)),threshold=0.3),"binaryRatingMatrix")
    #red <- as(diag(sqrt(red$d))%*%t(red$v),"binaryRatingMatrix")
    red <- as(biclust::binarize(diag(sqrt(red$d))%*%t(red$v),threshold=0.1),"binaryRatingMatrix")
    red@data@itemInfo <- data@data@itemInfo
    train.set <- red
    #)
  #}else{
    #model <- train.set
    #model.sample <- model[sample(1:nrow(musers), modelsize), ]
  }

  #time_usage("Reduction", time_reduce)


  modelsize <- min(nrow(train.set),modelsize)
  model <- train.set[sample(1:nrow(train.set), modelsize), ]


  testsize <- min(nrow(test.set),testsize)
  test.set <- test.set[sample(1:nrow(test.set), testsize), ]

  train <- model

  if(is.null(oldmodel)){
  time_model <- system.time(
    #if(alg=="IBCF"){
    #  r <- recom.ibcf2
    #}else{
      r <- Recommender(train, alg, parameter=parameter)
    #}
  )
  }else r <- oldmodel
  time_usage("Model", time_model)


  #partition data for predict

  parts <- slice(1:nrow(data),partsize)
  topNlist <- list()
  #print("parts:")
  #print(parts)

  time_predict <- system.time(
  for(part in parts){
    #cat("part#: ",part,"\n",sep="")
    topNlist <- c(topNlist,
                  as(predict(r, data[part], n=max(n),type="topNList"),"list"))
  }
  )
  time_usage("Prediction", time_predict)

  dt.topN <- data.table(user=all.users,top=topNlist)


  if(nrow(test.set)<10){
    print("Skiping evaluation. Test set too small")
    eval.alg <- F
  }
  if(!eval.alg) return(list(dt.topN,NULL))

  # eval ----

  #print(nrow(test.set))
  eval.scheme <- evaluationScheme(test.set,method="split",train=0.1,given=2)
  #train <- getData(scheme, type="train")
  test_known <- getData(eval.scheme, type="known")
  test_unknown <- getData(eval.scheme, type="unknown")

  cm <- matrix(NA, nrow=length(n), ncol=9,
               dimnames= list(n=n,
                              c("TP", "FP", "FN", "TN", "PP", "recall","precision","FPR","TPR")))



    topN <- predict(r, test_known, n=max(n),type="topNList")

  time_evaluate <- system.time(
  for(i in 1:length(n)) {
  #for(i in 1:1) {
    NN <- n[i]

    pred <- bestN(topN, NN)

    tp <- Matrix::rowSums(as(pred, "ngCMatrix") * as(test_unknown, "ngCMatrix"))

    pred_known <- Matrix::rowSums(as(pred, "ngCMatrix") * as(test_known, "ngCMatrix"))
    if(any(pred_known>0)) warning(paste("The algorithm ",
                                        r@model," predicted known items!!!"))

    tp_fn <- rowCounts(test_unknown)
    tp_fp <- rowCounts(pred)

    cm[i, "TP"] <- mean(tp)
    cm[i, "FP"] <- mean(tp_fp - tp)
    cm[i, "FN"] <- mean(tp_fn - tp)

    cm[i, "TN"] <- ncol(train) - mean(eval.scheme@given) + mean(pred_known) - cm[i, "TP"] -  cm[i, "FP"] - cm[i, "FN"]
    cm[i, "PP"] <- mean(tp_fp)

    cm[i, "precision"] <- cm[i, "TP"] / (cm[i, "TP"] + cm[i, "FP"])
    cm[i, "recall"] <- cm[i, "TP"] / (cm[i, "TP"] + cm[i, "FN"])
    cm[i, "TPR"] <- cm[i, "recall"]
    cm[i, "FPR"] <- cm[i, "FP"] / (cm[i, "FP"] + cm[i, "TN"])

  }
  )

  time_usage("Evaluation", time_evaluate)


  return(list(dt.topN,
              #if(keepModel) getModel(r) else NULL,
              if(keepModel) r else NULL,
              cm))

#   return(list(dt.topN,
#               new("confusionMatrix", cm = cm, model =
#                                           if(keepModel) getModel(r) else NULL)))
}


#' Popularity based recommendations
#'
#' @param data A binaryRatingMatrix.
#' @param n The number of recommendations we want for each user.
#' @param keepModel If TRUE the function will return the computed model
#' @param oldmodel A previously computed model to be used for predicitons
#' @param modelsize The maximum size for the model.
#' @param testsize The maximum size for the test set.
#' @param partsize The size for prediction partitions.
#' @param eval.alg If TRUE the function will return the evaluation confusion matrix
#'
#' @return A three element list:
#'  [[1]] contains the recommendations;
#'  [[2]] contains the model if \code{keepModel} is TRUE;
#'  [[3]] contains a confusion matrix if \code{eval.alg} is TRUE.
#' @examples
#' popularRec(data=ratings.sample,n=5)
#' popularRec(data=ratings.sample,n=5,modelsize=20000,testsize=1000,partsize=10000,eval.alg=T,keepModel=T)
#'
#' @export
popularRec <- function(data,n,modelsize=1000,testsize=500,partsize=10000,eval.alg=F,keepModel=F,oldmodel=NULL){
  recommend(data=data,alg="POPULAR",modelsize=modelsize,testsize=testsize,partsize=partsize,eval.alg=eval.alg,n=n,keepModel=keepModel,oldmodel=oldmodel)
}

#' Random recommendations
#'
#' @param data A binaryRatingMatrix.
#' @param n The number of recommendations we want for each user.
#' @param keepModel If TRUE the function will return the computed model
#' @param oldmodel A previously computed model to be used for predicitons
#' @param modelsize The maximum size for the model.
#' @param testsize The maximum size for the test set.
#' @param partsize The size for prediction partitions.
#' @param eval.alg If TRUE the function will return the evaluation confusion matrix
#'
#' @return A three element list:
#'  [[1]] contains the recommendations;
#'  [[2]] contains the model if \code{keepModel} is TRUE;
#'  [[3]] contains a confusion matrix if \code{eval.alg} is TRUE.
#' @examples
#' randomRec(data=ratings.sample,n=5)
#' randomRec(data=ratings.sample,n=5,modelsize=20000,testsize=1000,partsize=10000,eval.alg=T,keepModel=T)
#'
#' @export
randomRec <- function(data,n,modelsize=1000,testsize=500,partsize=10000,eval.alg=F,keepModel=F,oldmodel=NULL){
  recommend(data=data,alg="Random",modelsize=modelsize,testsize=testsize,partsize=partsize,eval.alg=eval.alg,n=n,keepModel=keepModel,oldmodel=oldmodel)
}

#' User based collaborative filtering recommendations
#'
#' @param data A binaryRatingMatrix.
#' @param n The number of recommendations we want for each user.
#' @param keepModel If TRUE the function will return the computed model
#' @param oldmodel A previously computed model to be used for predicitons
#' @param modelsize The maximum size for the model.
#' @param testsize The maximum size for the test set.
#' @param partsize The size for prediction partitions.
#' @param eval.alg If TRUE the function will return the evaluation confusion matrix
#'
#' @return A three element list:
#'  [[1]] contains the recommendations;
#'  [[2]] contains the model if \code{keepModel} is TRUE;
#'  [[3]] contains a confusion matrix if \code{eval.alg} is TRUE.
#' @examples
#' ubcfRec(data=ratings.sample,n=5)
#' ubcfRec(data=ratings.sample,n=5,modelsize=20000,testsize=1000,partsize=10000,eval.alg=T,keepModel=T)
#'
#' @export
ubcfRec <- function(data,n,modelsize=1000,testsize=500,partsize=10000,eval.alg=F,keepModel=F,oldmodel=NULL){
  recommend(data=data,alg="UBCF",modelsize=modelsize,testsize=testsize,partsize=partsize,eval.alg=eval.alg,n=n,keepModel=keepModel,,oldmodel=oldmodel,parameter = list(nn=50,method = "jaccard"))
}

#' Item based collaborative filtering recommendations
#'
#' @param data A binaryRatingMatrix.
#' @param n The number of recommendations we want for each user.
#' @param keepModel If TRUE the function will return the computed model
#' @param oldmodel A previously computed model to be used for predicitons
#' @param modelsize The maximum size for the model.
#' @param testsize The maximum size for the test set.
#' @param partsize The size for prediction partitions.
#' @param eval.alg If TRUE the function will return the evaluation confusion matrix
#'
#' @return A three element list:
#'  [[1]] contains the recommendations;
#'  [[2]] contains the model if \code{keepModel} is TRUE;
#'  [[3]] contains a confusion matrix if \code{eval.alg} is TRUE.
#' @examples
#' ibcfRec(data=ratings.sample,n=5)
#' ibcfRec(data=ratings.sample,n=5,modelsize=20000,testsize=1000,partsize=10000,eval.alg=T,keepModel=T)
#'
#' @export
ibcfRec <- function(data,n,modelsize=1000,testsize=500,partsize=10000,eval.alg=F,keepModel=F,oldmodel=NULL){
  recommend(data=data,alg="IBCF",modelsize=modelsize,testsize=testsize,partsize=partsize,eval.alg=eval.alg,n=n,keepModel=keepModel,oldmodel=oldmodel,parameter = list(method = "Jaccard"))
}

#' User based collaborative filtering recommendations (with training set dimensionality reduction)
#'
#' @param data A binaryRatingMatrix.
#' @param n The number of recommendations we want for each user.
#' @param keepModel If TRUE the function will return the computed model
#' @param oldmodel A previously computed model to be used for predicitons
#' @param modelsize The maximum size for the model.
#' @param testsize The maximum size for the test set.
#' @param partsize The size for prediction partitions.
#' @param eval.alg If TRUE the function will return the evaluation confusion matrix
#'
#' @return A three element list:
#'  [[1]] contains the recommendations;
#'  [[2]] contains the model if \code{keepModel} is TRUE;
#'  [[3]] contains a confusion matrix if \code{eval.alg} is TRUE.
#' @examples
#' ubcfredRec(data=ratings.sample,n=5)
#' ubcfredRec(data=ratings.sample,n=5,modelsize=20000,testsize=1000,partsize=10000,eval.alg=T,keepModel=T)
#'
#' @export
ubcfredRec <- function(data,n,modelsize=1000,testsize=500,partsize=10000,eval.alg=F,keepModel=F,oldmodel=NULL){
  recommend(data=data,alg="UBCF",reduction=T,modelsize=modelsize,testsize=testsize,partsize=partsize,eval.alg=eval.alg,n=n,keepModel=keepModel,oldmodel=oldmodel,parameter = list(method = "Jaccard"))
}


#' SVD dimensionality reduction based recommendations
#'
#' @param data A binaryRatingMatrix.
#' @param n The number of recommendations we want for each user.
#' @param keepModel If TRUE the function will return the computed model
#' @param oldmodel A previously computed model to be used for predicitons
#' @param modelsize The maximum size for the model.
#' @param testsize The maximum size for the test set.
#' @param partsize The size for prediction partitions.
#' @param eval.alg If TRUE the function will return the evaluation confusion matrix
#'
#' @return A three element list:
#'  [[1]] contains the recommendations;
#'  [[2]] contains the model if \code{keepModel} is TRUE;
#'  [[3]] contains a confusion matrix if \code{eval.alg} is TRUE.
#' @examples
#' svdRec(data=ratings.sample,n=5)
#' svdRec(data=ratings.sample,n=5,modelsize=20000,testsize=1000,partsize=10000,eval.alg=T,keepModel=T)
#'
#' @export
svdRec <- function(data,n,modelsize=1000,testsize=500,partsize=10000,eval.alg=F,keepModel=F,oldmodel=NULL){
  recommend(data=data,alg="binSVD",reduction=F,modelsize=modelsize,testsize=testsize,partsize=partsize,eval.alg=eval.alg,n=n,keepModel=keepModel,oldmodel=oldmodel)
}


