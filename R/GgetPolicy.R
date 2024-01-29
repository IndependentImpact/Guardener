#' GgetPolicy
#'
#' @param refreshToken Character. JWT refresh token returned by Glogin()$refreshToken.
#' @param baseurl Character. Base url. defaults to "http://localhost:3000/"
#' @param policyId Character. Typically from the $id item of the result of GgetPolicies()
#' @param returndf Return a tibble or not Default FALSE.
#' @return tibble or list
#' @export

GgetPolicy <- function(refreshToken,
                       baseurl = "http://localhost:3000/",
                       policyId = NULL,
                       returndf = FALSE){

  if (is.null(policyId)) stop("I need a policyID")

  # Get access token for the request.
  accessToken <- GgetAccessToken(refreshToken = refreshToken,
                                 baseurl = baseurl)

  # Make the request for the policy.
  url <- sprintf("%sapi/v1/policies/%s", baseurl, policyId)
  raw <- httr::GET(
    url = url,
    httr::add_headers(Authorization = sprintf("Bearer %s", accessToken)))

  if (raw$status_code != 200) {
    stop(sprintf("Error: Query returned status code %s.", raw$status_code))
  }

  res <-  httr::content(raw)
  res$config <- assignNames(res$config)
  if (returndf){
    df <- Glist2tibble(res)
    return(df)
  }
  return(res)
}
