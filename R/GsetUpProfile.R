
#' @title GsetUpProfile
#' @description Associates a Guardian user with a Hedera account and a Guardian
#'  standard registry user.
#' @param refreshToken Character. JWT refresh token returned by Glogin()$refreshToken.
#' @param un Character. The username to be registered.
#' @param role Character. The Guardian user type to be registered. Defaults to "USER".
#' @param parentDid Character. The DID of the standard registry user to associate the user with.
#' @param hederaAccountId Character. The Hedera account ID to be associated with the user.
#' @param hederaAccountKey Character. The Hedera account key to be associated with the user.
#' @param baseurl Character. URL where /api/ is hosted. Default "http://localhost:3000/".
#' @export
#'
GsetUpProfile <- function(refreshToken,
                          un,
                          role,
                          parentDid,
                          hederaAccountId,
                          hederaAccountKey,
                          baseurl = "http://localhost:3000/") {

  # Get access token for this request.
  accessToken <- Guardener::GgetAccessToken(
    refreshToken = refreshToken,
    baseurl = baseurl)

  # Make the request.
  res <- httr::PUT(url = sprintf("%sapi/v1/profiles/%s", baseurl, un),
                   httr::add_headers(
                     Authorization = sprintf("Bearer %s", accessToken)),
                   body = list(
                     username = un,
                     role = role,
                     parent = parentDid,
                     hederaAccountId = hederaAccountId,
                     hederaAccountKey = hederaAccountKey))

  # Process the result.
  {
    if (res$status_code != 204) {
      errMsg <- httr::content(res, as = "parsed")
      stop(errMsg)
    }

    res <- httr::content(res, as = "parsed") # This returns NULL if they API request was successful.
  }

  # Done.
  return(res)
}
