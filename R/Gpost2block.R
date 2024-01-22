
#' Gpost2block
#'
#' @param refreshToken Character. JWT refresh token returned by Glogin()$refreshToken.
#' @param document Named list or data frame with data to submit. Names must correspond to field names in Schema.
#' @param schemaID character. Schema to submit to.
#' @param schemaIPFSurl character. Schema to submit to.
#' @param policyId character. Schema to submit to.
#' @param BlockId character. Schema to submit to.
#' @param baseurl Character. URL where /api/ is hosted. Default "http://localhost:3000/".
#' @return NULL. #TODO. This is not accurate. It does return stuff.
#' @export

Gpost2block <- function(refreshToken,
                        document,
                        schemaID,
                        schemaIPFSurl,
                        policyId,
                        BlockId,
                        baseurl = "http://localhost:3000/"){

  # Get access token for this query.
  accessToken <- GgetAccessToken(refreshToken = refreshToken,
                                 baseurl = baseurl)

  # Make the query.
  httr::POST(
    url = sprintf("%sapi/v1/policies/%s/blocks/%s", baseurl, policyId, BlockId),
    httr::add_headers(Authorization = sprintf("Bearer %s", accessToken)),
    body = GmakeBody(document, schemaID, schemaIPFSurl),
    encode = "json")
}

