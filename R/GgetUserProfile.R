#' GgetUserProfile
#'
#'@description Get user profile.
#'@param refreshToken Character. JWT refresh token returned by Glogin()$refreshToken.
#'@param un Character. Username of the user for whom to retrieve the profile.
#'@param baseurl Character. Base url. Defaults to "http://localhost:3000/".
#'@return Data frame.
#'@export

GgetUserProfile <- function(refreshToken,
                            un,
                            baseurl = "http://localhost:3000/") {

  # Get access token for this query.
  accessToken <- GgetAccessToken(refreshToken = refreshToken,
                                 baseurl = baseurl)

  # Make the query.
  res <- httr::GET(url = sprintf("%sapi/v1/profiles/%s",
                                 baseurl,
                                 gsub(pattern = "[[:blank:]]",
                                      replacement = "%20",
                                      x = un)),
                   httr::add_headers(Authorization = sprintf("Bearer %s",
                                                             accessToken)))
  if (res$status_code != 200) {
    stop(sprintf("Failed to retrieve user profile. status_code: %s",
                 res$status_code))
  }

  # Process the result.
  res <- httr::content(res, as = "parsed")

  # Retrieve the message IDs of any included documents.
  res[c("vcDocMessageId", "didDocMessageId")] <- NA_character_
  idx <- which(names(res) == "vcDocument")
  if (length(idx) == 1) {
    res$vcDocMessageId <- res[[idx]]$messageId
  }
  idx <- which(names(res) == "didDocument")
  if (length(idx) == 1) {
    res$didDocMessageId <- res[[idx]]$messageId
  }

  # Remove list columns for simplicity.
  idxx <- which(sapply(X = res, FUN = is.list))
  if (length(idxx) > 0) {
    res <- res[-idxx]
  }

  # Remove NULL columns.
  idxx <- which(sapply(X = res, FUN = is.null))
  if (length(idxx) > 0) {
    res <- res[-idxx]
  }

  # Convert to data frame.
  res <- data.frame(res)

  return(res)
}
