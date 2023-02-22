#' GgetPolicies
#' @description Get the policies associated with an user identified by an access token or username and passwork
#' @param accessToken Character. Access token returned by Glogin()  $accessToken
#' @param baseurl Character. Base url. defaults to "http://localhost:3000/"
#' @param un Character. Username
#' @param pw Character. Password
#' @param pageIndex Integer. default 0
#' @param pageSize Integer. default 10
#' @param returndf Logical. Returns a tibble if TRUE. Else a list (default)
#'
#' @return tibble or list
#' @export

GgetPolicies <- function(accessToken = NULL,
                         un = NULL,
                         pw = NULL,
                         baseurl = "http://localhost:3000/",
                         pageIndex = 0,
                         pageSize = 10,
                         returndf = FALSE,
                         verbose = FALSE){

  if (is.null(accessToken)){
    if (is.null(un) | is.null(pw)) {
      stop("When accessToken is NULL, you should give a username and password (un and pw). \n I can't perform miracles")}
  }

  policies <- httr::GET(url = sprintf("%sapi/v1/policies", baseurl),
                  pageIndex = pageIndex,
                  pageSize = pageSize,
                  httr::add_headers(Authorization = sprintf("Bearer %s", accessToken)))
  res <- httr::content(policies)

  if (returndf) {
    df <- Glist2tibble(res, verbose = verbose)
    return(df)
  }

  res
}
