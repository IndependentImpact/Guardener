#' @title GcopyPolicy
#' @description Makes a copy of a Guardian policy and pushes it back to the
#'    Guardian instance as a draft policy.
#' @param refreshToken Character. JWT refresh token returned by Glogin()$refreshToken.
#' @param messageId Character. The message ID of the policy to be copied. If provided,
#'    the function will ignore the 'fp' argument. If not provided, the function will
#'    try to copy the policy from the ZIP file provided in the 'fp' argument.
#' @param fp Character. The file path of the ZIP file of the policy to be copied.
#'    Ignored if 'messageId' is provided.
#' @param baseurl Character. Base URL of the targeted Guardian instance. Defaults
#'   to "http://localhost:3000/".
#' @return Character. The Guardian policy ID of the newly created copy.
#' @export
#'
GcopyPolicy <- function(refreshToken,
                        messageId = NULL,
                        fp = NULL,
                        baseurl = "http://localhost:3000/") {

  # Input checking.
  {
    if (length(messageId) == 0 & length(fp) == 0) {
      stop("Either 'messageId' or 'fp' must be provided.")
    }
    if (length(messageId) == 1 & length(fp) == 1) {
      stop("Both 'messageId' and 'fp' provided; 'fp' will be ignored.")
    }
  }

  # Initialise return variable.
  policyId <- NULL

  # Get access token for this request.
  accessToken <- GgetAccessToken(
    refreshToken = refreshToken,
    baseurl = baseurl)

  # Submit the copy request to Guardian.
  if (length(messageId) == 1) { # Copy via message ID.

    res <- httr::POST(
      url = sprintf("%sapi/v1/policies/push/import/message", baseurl),
      httr::add_headers(
        Authorization = sprintf("Bearer %s", accessToken)),
      body = list(messageId = messageId))

  } else { # Copy via file.

    res <- httr::POST(
      url = sprintf("%sapi/v1/policies/push/import/file", baseurl),
      httr::add_headers(
        Authorization = sprintf("Bearer %s", accessToken)),
      httr::content_type("binary/octet-stream"),
      body = readBin(con = fp, what = "raw", n = file.size(fp)),
      encode = "raw")

  }

  # Process the result.
  {
    if (res$status_code != 202) {
      stCode <- res$status_code
      errMsg <- httr::content(res, as = "parsed")
      stop(sprintf("Failed to make policy copy: %s (%s)",
                   errMsg, stCode))
    }

    res <- httr::content(res, as = "parsed")
  }

  # Wait for the task to finish.
  {
    taskId <- res$taskId

    bDone <- FALSE
    bTimeOut <- FALSE # We'll time-out after 5 minutes.
    tsStart <- Sys.time()
    while (!bTimeOut & !bDone) {

      # Give the Guardian some time.
      Sys.sleep(5)

      # Get an update on the task status.
      res <- Guardener::GgetTaskStatus(
        refreshToken = refreshToken,
        baseurl = baseurl,
        taskId = taskId)

      if ("error" %in% names(res)) {
        if (length(res$error) > 0) {
          if (length(res$error$code) > 0 || length(res$error$message) > 0) {
            stop(sprintf("Failed to make policy copy: %s (%s)",
                         res$error$message, res$error$code))
          }
        }
      }

      # Try to get the policyId of the new copy.
      if ("result" %in% names(res)) {
        if ("policyId" %in% names(res$result)) {
          if (length(res$result$policyId) > 0) {
            if (nchar(res$result$policyId) >= 24) {
              policyId <- res$result$policyId
              bDone <- TRUE
            }
          }
        }
      }

      # Time-out check.
      if (!bDone) {
        bTimeOut <- as.integer(
          difftime(
            time1 = Sys.time(),
            time2 = tsStart,
            units = "secs")) > (5*60)
      }
    }
  }

  # Return the policyId.
  return(policyId)

}


