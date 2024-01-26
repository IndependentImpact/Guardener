#' GgetBlockDocuments
#'
#' @description
#' @param refreshToken Character. JWT refresh token returned by Glogin()$refreshToken.
#' @param policyId Character. ID of the policy from which to retrieve the documents.
#' @param blockId Character. ID of the block from which to retrieve the documents.
#' @param baseurl Character. URL where /api/ is hosted. Default "http://localhost:3000/".
#' @param inclFileMd Logical. If TRUE, file metadata is included in the returned
#'  list. Defaults to TRUE.
#' @return List. List of documents associated with the block.
#' @export

GgetBlockDocuments <- function(refreshToken,
                               policyId,
                               blockId,
                               baseurl = "http://localhost:3000/",
                               inclFileMd = TRUE) {

  # Get access token for this query.
  accessToken <- GgetAccessToken(refreshToken = refreshToken,
                                 baseurl = baseurl)

  # Make the query.
  res <- httr::GET(url = sprintf("%sapi/v1/policies/%s/blocks/%s",
                                 baseurl, policyId, blockId),
                   httr::add_headers(Authorization = sprintf("Bearer %s", accessToken)))

  if (res$status_code != 200) {
    stop(sprintf("Failed to retrieve documents. Status code: %s",
                 res$status_code))
  }

  # Process the result.
  {
    res <- httr::content(x = res, as = "parsed")
    lsDocs <- res$data

    # Unbox vectors of length 1 on all levels of the list.
    lsDocs <- jsonlite::toJSON(x = lsDocs, auto_unbox = TRUE)
    lsDocs <- jsonlite::fromJSON(
      lsDocs,
      simplifyVector = TRUE,
      flatten = TRUE,
      simplifyDataFrame = FALSE)

    # Strip file metadata, if requested.
    if (!inclFileMd) {
      lsDocs <- lapply(X = lsDocs, FUN = function(x) {
        return(x$document)
      })
    }

    return(lsDocs)
  }
}

# TODO: Implement pagination.
