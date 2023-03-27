
#' GgetPolicy
#'
#'@param accessToken Character. Access token returned by Glogin()  $accessToken
#' @param un Character. Username
#' @param pw Character. Password
#' @param baseurl Character. Base url. defaults to "http://localhost:3000/"
#' @param policyId Character. Typically from the $id item of the result of GgetPolicies()
#' @param returndf Return a tibble or not Default FALSE.
#' @return tibble or list
#' @export

GgetPolicy <- function(accessToken = NULL,
                       un = NULL,
                       pw = NULL,
                       baseurl = "http://localhost:3000/",
                       policyId = NULL,
                       returndf = FALSE){
  if (is.null(accessToken)){
    if (is.null(un) | is.null(pw)) {
      stop("When accessToken is NULL, you should give a username and password (un and pw).
           I can't perform miracles")}
  }
  if (is.null(policyId)) stop("I need a policyID")
  url <- sprintf("%sapi/v1/policies/%s", baseurl, policyId)

  raw <- httr::GET(url = url, httr::add_headers(Authorization = sprintf("Bearer %s", accessToken)))
  res <-  httr::content(raw)
  res$config <- assignNames(res$config)
  if (returndf){
    df <- Glist2tibble(res)
    return(df)
  }
  res
}
