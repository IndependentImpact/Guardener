#' @title GrenamePolicy
#' @description Renames a draft policy on the Guardian instance..
#' @param refreshToken Character. JWT refresh token returned by Glogin()$refreshToken.
#' @param policyId Character. The Guardian ID of the draft policy to rename.
#' @param newName Character. The new name for the policy.
#' @param baseurl Character. Base URL of the targeted Guardian instance. Defaults
#'   to "http://localhost:3000/".
#' @return Logical. TRUE if the policy was renamed successfully; will throw an
#'   error otherwise.
#' @export
#'
GrenamePolicy <- function(refreshToken,
                          policyId,
                          newName = NULL,
                          baseurl = "http://localhost:3000/") {

  # Input checking.
  {
    if (length(newName) > 1) {
      warning("length(newName) > 1. Only the first element will be used.")
      newName <- newName[[1]]
    }
    if (length(newName) == 1) {
      if (nchar(newName) == 0) {
        newName <- NULL
      }
    }
  }

  # Get the policy's config.
  polConf <- Guardener::GgetPolicy(
    refreshToken = refreshToken,
    baseurl = baseurl,
    policyId = policyId,
    assignNms = FALSE)

  # Change the name.
  polConf$name <- newName

  # Get access token for next request.
  accessToken <- GgetAccessToken(
    refreshToken = refreshToken,
    baseurl = baseurl)

  # Push name change to Guardian instance.
  res <- httr::PUT(
    url = sprintf("%sapi/v1/policies/%s", baseurl, policyId),
    httr::add_headers(
      Authorization = sprintf("Bearer %s", accessToken)),
    httr::content_type("application/json"),
    encode = "raw",
    body = jsonlite::toJSON(
      x = polConf,
      factor = "string",
      complex = "string",
      auto_unbox = TRUE, # The request fails when auto_unbox = FALSE.
      pretty = FALSE,
      force = FALSE))

  # Process the result.
  if (res$status_code != 200) {
    stCode <- res$status_code
    errMsg <- httr::content(res, as = "parsed")
    stop(sprintf("Failed to change policy name: %s (%s)",
                 errMsg, stCode))
  }

  # Done.
  return(TRUE)

}


