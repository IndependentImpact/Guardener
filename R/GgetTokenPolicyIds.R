#' GgetTokenPolicyIds
#'
#'@description Get the policy IDs in which each token is used. All tokens
#'  accessible to the current user will be searched.
#'@param refreshToken Character. JWT refresh token returned by Glogin()$refreshToken.
#'@param baseurl Character. Base url. Defaults to "http://localhost:3000/".
#'@return Data frame. A data frame mapping token IDs to policy IDs.
#'@export
#'
GgetTokenPolicyIds <- function(refreshToken,
                               baseurl) {

  rbind.fill <- plyr::rbind.fill

  # Get access token for this query.
  accessToken <- GgetAccessToken(refreshToken = refreshToken,
                                 baseurl = baseurl)

  # Make the query.
  res <- httr::GET(url = sprintf("%sapi/v1/tokens", baseurl),
                   httr::add_headers(Authorization = sprintf("Bearer %s",
                                                             accessToken)))
  if (res$status_code != 200) {
    stop(sprintf("Failed to retrieve list of tokens. status_code: %s",
                 res$status_code))
  }

  # Process the result.
  {
    res <- httr::content(res, as = "parsed")

    res <- lapply(X = res, FUN = function(x) {
      if (length(x$policyIds) == 0) {
        return(data.frame(
          id_token = x$id,
          id_policy = NA_character_))
      }
      return(data.frame(
        id_token = x$id,
        id_policy = x$policyIds[[1]]))
    })

    res <- do.call("rbind.fill", res)
  }

  return(res)
}
