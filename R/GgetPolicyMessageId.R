
#' @title GgetPolicyMessageId
#' @description Gets the message ID of a policy.
#' @param policyId The ID of the policy.
#' @param refreshToken Character. JWT refresh token returned by Glogin()$refreshToken.
#' @param baseurl Character. Base url of the targeted Guardian instance.
#'    Defaults to "http://localhost:3000/".
#' @return Character. The message ID of the policy.
#' @export
#'
GgetPolicyMessageId <- function(policyId,
                                refreshToken,
                                baseurl = "http://localhost:3000/") {

  # Get access token for this query.
  accessToken <- Guardener::GgetAccessToken(
    refreshToken = refreshToken,
    baseurl = baseurl)

  # Make the query.
  url <- sprintf("%sapi/v1/policies/%s/export/message",
                 baseurl, policyId)
  res <- httr::GET(url = url,
                   httr::add_headers(Authorization =
                                       sprintf("Bearer %s", accessToken)))

  if (res$status_code != 200) {
    stCode <- res$status_code
    errMsg <- httr::content(res, as = "parsed")
    stop(sprintf("Failed to retrieve policy's message ID: %s (%s)",
                 errMsg, stCode))
  }

  # Process the result.
  res <- httr::content(res, as = "parsed")

  return(res$messageId)
}
