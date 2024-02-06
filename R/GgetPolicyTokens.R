#' GgetPolicyTokens
#'
#'@description Get a list of tokens in use in a specific policy.
#'@param policyId Character. Policy ID to get token info for.
#'@param refreshToken Character. JWT refresh token returned by Glogin()$refreshToken.
#'@param baseurl Character. Base url. Defaults to "http://localhost:3000/".
#'@return Data frame.
#'@export

GgetPolicyTokens <- function(refreshToken,
                             policyId,
                             baseurl = "http://localhost:3000/") {

  rbind.fill <- plyr::rbind.fill

  # Get access token for this query.
  accessToken <- GgetAccessToken(refreshToken = refreshToken,
                                 baseurl = baseurl)

  # Make the query.
  res <- httr::GET(url = sprintf("%sapi/v1/tokens?policy=%s",
                                 baseurl, policyId),
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

      # Remove NULL fields.
      x <- x[which(!sapply(x, is.null))]

      # Remove list fields.
      x <- x[which(!sapply(x, is.list))]

      return(data.frame(x, stringsAsFactors = FALSE))
    })

    res <- do.call("rbind.fill", res)

    if ("wipeContractId" %in% names(res)) {
      res$wipeContractId[which(nchar(res$wipeContractId) == 0)] <- NA_character_
    }
  }

  return(res)
}
