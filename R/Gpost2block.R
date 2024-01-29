
#' Gpost2block
#'
#' @param refreshToken Character. JWT refresh token returned by Glogin()$refreshToken.
#' @param x List. Data to submit to the policy block. If x is a document based
#'   on a schema, the document should already contain appropriately populated
#'   'context' and 'type' fields.
#' @param policyId character. Schema to submit to.
#' @param blockId character. Schema to submit to.
#' @param baseurl Character. URL where /api/ is hosted. Default "http://localhost:3000/".
#' @return List. Some metadata about the submission.
#' @export

Gpost2block <- function(x,
                        refreshToken,
                        policyId,
                        blockId,
                        baseurl = "http://localhost:3000/"){

  # Get access token for this query.
  accessToken <- GgetAccessToken(refreshToken = refreshToken,
                                 baseurl = baseurl)

  # Make the query.
  res <- httr::POST(
    url = sprintf("%sapi/v1/policies/%s/blocks/%s", baseurl, policyId, blockId),
    httr::add_headers(Authorization = sprintf("Bearer %s", accessToken)),
    body = x,
    encode = "json")

  if (!res$status_code == 200) {
    stop(sprintf("ERROR. Failed to post to block. Status code: %s",
                 res$status_code))
  }

  res <- httr::content(res, as = "parsed")

  return(res)
}

# "@context": [
#   "ipfs://bafkreifadsohb4fomdgqrftlqdlzdx7qhar7nfxdy76bulcqpawghu4poy"
# ],
# "type": "9d164446-9596-497d-8990-3ceefa7b8a62&1.0.0"

