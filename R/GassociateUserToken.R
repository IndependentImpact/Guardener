
#' @title GassociateUserToken
#' @description Associates a Guardian user with a token of a Guardian policy.
#' @param refreshToken Character. JWT refresh token returned by Glogin()$refreshToken.
#' @param tokenId Character. The ID of the token, e.g., "0.0.2950835".
#' @param baseurl Character. URL where /api/ is hosted. Default "http://localhost:3000/".
#' @return Logical. TRUE if successful.
#' @export
GassociateUserToken <- function(refreshToken,
                                tokenId,
                                baseurl = "http://localhost:3000/") {


  # First check that the user actually has access to the token. If the user is
  # not associated with a policy that has access to the token, then the user
  # won't be able to associate with the token.
  dfTkns <- GgetTokens(refreshToken = refreshToken,
                       baseurl = baseurl)
  if (!tokenId %in% dfTkns$tokenId) {
    stop(sprintf("User is not associated with any of the policies of token %s.",
                 tokenId))
  }

  # Also check if the user has not already been associated with this token,
  # because the Guardian API throws an error if the user is already associated.
  if (dfTkns$associated[which(dfTkns$tokenId == tokenId)]) {
    return(TRUE)
  }

  # Get access token for this query.
  accessToken <- GgetAccessToken(refreshToken = refreshToken,
                                 baseurl = baseurl)

  # Make the query.
  res <- httr::PUT(url = sprintf("%sapi/v1/tokens/%s/associate",
                                 baseurl, tokenId),
                   httr::add_headers(Authorization = sprintf("Bearer %s",
                                                             accessToken)),
                   encode = "json")
  if (res$status_code != 200) {
    stop(sprintf("Failed to associate user with token %s. status_code: %s",
                 tokenId, res$status_code))
  }

  # Process the result.
  res <- httr::content(res, as = "parsed")

  # Done.
  return(res$status)
}
