#' GgetUserTokenInfo
#'
#' @description Returns user information for the selected token. Only users with
#'    the Standard Registry role are allowed to make the request.
#' @param refreshToken Character. JWT refresh token returned by Glogin()$refreshToken.
#' @param baseurl Character. Base URL of the targeted Guardian instance. Defaults
#'   to "http://localhost:3000/".
#' @param tokenId Character. The ID of the token of interest, e.g., "0.0.7717332".
#' @param usernm Character. The username of the user of interest.
#' @return Data frame.
#' @export

GgetUserTokenInfo <- function(refreshToken,
                              tokenId,
                              usernm,
                              baseurl = "http://localhost:3000/") {

  # Get access token for this query.
  accessToken <- GgetAccessToken(refreshToken = refreshToken,
                                 baseurl = baseurl)

  # Make the query.
  res <- httr::GET(url = sprintf("%sapi/v1/tokens/%s/%s/info",
                                 baseurl,
                                 tokenId,
                                 usernm),
                   httr::add_headers(Authorization = sprintf("Bearer %s",
                                                             accessToken)))
  if (res$status_code != 200) {
    stop(sprintf("Failed to retrieve user-token info for token %s and user %s. status_code: %s",
                 tokenId,
                 usernm,
                 res$status_code))
  }

  # Process the result.
  {
    res <- httr::content(res, as = "parsed")
    res <- res[which(!sapply(res, is.null))]
    res <- data.frame(res, stringsAsFactors = FALSE)
  }

  return(res)
}
