#'GgetAccounts
#'
#'@description Returns all users on the targeted Guardian instance, except those
#'   with roles Standard Registry and Auditor. Only users with the Standard
#'   Registry role are allowed to make the request.
#'@param refreshToken Character. JWT refresh token returned by Glogin()$refreshToken.
#'@param baseurl Character. Base URL of the targeted Guardian instance. Defaults
#'   to "http://localhost:3000/".
#'@return Data frame.
#'@export
#'
GgetAccounts <- function(refreshToken,
                         baseurl = "http://localhost:3000/") {

  rbind.fill <- plyr::rbind.fill

  # Get access token for this query.
  accessToken <- GgetAccessToken(refreshToken = refreshToken,
                                 baseurl = baseurl)

  # Make the query.
  res <- httr::GET(url = sprintf("%sapi/v1/accounts", baseurl),
                   httr::add_headers(Authorization = sprintf("Bearer %s",
                                                             accessToken)))
  if (res$status_code != 200) {
    stop(sprintf("Failed to retrieve list of accounts. status_code: %s",
                 res$status_code))
  }

  # Process the result.
  {
    res <- httr::content(res, as = "parsed")
    res <- lapply(X = res, FUN = function(x) {
      x <- x[which(!sapply(X = x, FUN = is.null))]
      return(data.frame(x, stringsAsFactors = FALSE))
    })
    res <- do.call("rbind.fill", res)
  }

  return(res)
}
