.BIN_SVD_param <- list(
)

.get_parameters <- function(defaults, parameter) {
  defaults <- as.list(defaults)
  parameter <- as.list(parameter)

  ## add verbose
  if(is.null(defaults$verbose)) defaults$verbose <- FALSE

  if(length(parameter) != 0) {
    o <- pmatch(names(parameter), names(defaults))

    ## unknown parameter
    if(any(is.na(o))){
      warning(sprintf(ngettext(length(is.na(o)),
                               "Unknown parameter: %s",
                               "Unknown parameters: %s"),
                      paste(names(parameter)[is.na(o)],
                            collapse = ", ")), call. = FALSE, immediate. = TRUE)

      cat("Available parameter (with default values):\n")
      #print(defaults)
      cat(rbind(names(defaults)," = ", gsub("\n"," ",as.character(defaults))),
          sep=c("\t"," ","\n"))
    }

    defaults[o[!is.na(o)]] <- parameter[!is.na(o)]
  }

  if(defaults$verbose) {
    cat("Used parameters:\n")
    #print(defaults)
    cat(rbind(names(defaults)," = ", gsub("\n"," ",as.character(defaults))),
        sep=c("\t"," ","\n"))
  }

  defaults
}


BIN_SVD <- function(data, newdata=NULL, parameter= NULL) {

  p <- .get_parameters(.BIN_SVD_param, parameter)

  model <- c(list(
    description = "full matrix",
    data = data
  ), p)

  predict <- function(model, newdata, n = 10,
                      data=NULL, type="topNList", ...) {


    n <- as.integer(n)

    users <- newdata@data@itemsetInfo$itemsetID
    items <- newdata@data@itemInfo$labels

    data <- Matrix::t(model$data@data@data)

    data <- rbind(Matrix::t(newdata@data@data), data)

    data <- as(data, "matrix")

    #print(sum(data[1,]))
    #data[is.na(data)] <- 0

    s <- svd(data)
    su <- s$u %*% diag(sqrt(s$d))
    sv <- diag(sqrt(s$d)) %*% Matrix::t(s$v)

    ratings <- su[1:nrow(newdata),] %*% sv
    #ratings <- biclust::binarize(ratings,threshold=0.1)


    rownames(ratings) <- as.vector(users)
    colnames(ratings) <- as.vector(items)
    #ratings <- as(ratings, "realRatingMatrix")
    ratings <- new("realRatingMatrix", data=dropNA(ratings))

    ratings <- removeKnownRatings(ratings, newdata)
    getTopNLists(ratings, n=n)

  }

  new("Recommender", method = "binSVD", dataType = class(data),
      ntrain = nrow(data), model = model, predict = predict)
}



