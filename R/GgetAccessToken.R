#' GgetAccessToken
#'
#' @param refreshToken Character. JWT refresh token returned by Glogin()$refreshToken.
#' @param baseurl Character. URL where /api/ is hosted. Default "http://localhost:3000/".
#' @return Character. A JWT access token valid for 30 seconds.
#' @export

GgetAccessToken <- function(refreshToken,
                            baseurl = "http://localhost:3000/") {

  res <- httr::POST(url = sprintf("%sapi/v1/accounts/access-token", baseurl),
             body = list(refreshToken = refreshToken))
  if (res$status_code != 201) {
    stop(sprintf("Error: Query returned status code %s.", res$status_code))
  }

  res <- httr::content(res, as = "parsed")
  return(res$accessToken)
}
