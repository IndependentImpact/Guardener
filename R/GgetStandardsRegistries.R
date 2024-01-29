#'GgetStandardsRegistries
#'
#'@description Get the names and DIDs of all standards registry users on the Guardian instance at <baseurl>.
#'@param refreshToken Character. JWT refresh token returned by Glogin()$refreshToken.
#'@param baseurl Character. Base url. Defaults to "http://localhost:3000/".
#'@return Data frame.
#'@export

GgetStandardsRegistries <- function(refreshToken,
                                    baseurl = "http://localhost:3000/") {

  rbind.fill <- plyr::rbind.fill

  # Get access token for this query.
  accessToken <- GgetAccessToken(refreshToken = refreshToken,
                                 baseurl = baseurl)

  # Make the query.
  res <- httr::GET(url = sprintf("%sapi/v1/accounts/standard-registries",
                                 baseurl),
                   httr::add_headers(Authorization = sprintf("Bearer %s",
                                                             accessToken)))
  if (res$status_code != 200) {
    stop(sprintf("Failed to retrieve list of policies. status_code: %s",
                 res$status_code))
  }

  # Process the result.
  lsSRs <- httr::content(x = res, as = "parsed")
  lsSRs <- lapply(X = lsSRs, FUN = data.frame, stringsAsFactors = FALSE)
  dfSRs <- do.call("rbind.fill", lsSRs)

  return(dfSRs)
}
