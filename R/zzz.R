.BIN_SVD_param <- list(
)


onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to RecommenderYang")
}


.onLoad <- function(libname, pkgname) {
  op <- options()
  op.recyang <- list(
    recyang.path = "~/R-dev",
    recyang.install.args = "",
    recyang.name = "RecommenderYang",
    recyang.desc.author = '"Antonio Moreira <antonio.moreira@ucp.pt> [aut, cre]"',
    recyang.desc.license = "license",
    recyang.desc.suggests = NULL,
    recyang.desc = list()
  )
  toset <- !(names(op.recyang) %in% names(op))
  if(any(toset)) options(op.recyang[toset])

  #register algs
  if(is.null(recommenderRegistry$get_entries(method="binSVD"))){
    recommenderRegistry$set_entry(
      method="binSVD", dataType = "binaryRatingMatrix", fun=BIN_SVD,
      description="Recommender based on SVD dimensionality reduction.",
      parameters = .BIN_SVD_param)
  }else {
    recommenderRegistry$modify_entry(
      method="binSVD", dataType = "binaryRatingMatrix", fun=BIN_SVD,
      description="Recommender based on SVD dimensionality reduction.",
      parameters = .BIN_SVD_param)
  }

  invisible()
}

.onUnload <- function(libname, pkgname) {
}


