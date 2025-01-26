#' @title GpublishPolicy
#' @description Changes a draft policy's status to 'PUBLISH' in the Guardian.
#' @param refreshToken Character. JWT refresh token returned by Glogin()$refreshToken.
#' @param policyId Character. The Guardian ID of the draft policy to rename.
#' @param newVersionNo Character. Optional. The new version number of the policy. If
#'  not provided and publish = TRUE, the function will try to determine the new
#'  version number automatically. It is strongly recommended to provide the correct
#'  version number yourself. This argument is ignored if publish = FALSE.
#' @param baseurl Character. Base URL of the targeted Guardian instance. Defaults
#'   to "http://localhost:3000/".
#' @return Logical. TRUE if the policy was successfully published; will throw
#'   an error otherwise.
#' @export
#'
GpublishPolicy <- function(refreshToken,
                           policyId,
                           newVersionNo = NULL,
                           baseurl = "http://localhost:3000/") {

  # Input checking.
  {
    if (length(newVersionNo) > 1) {
      warning("length(newVersionNo) > 1. Only the first element will be used.")
      newVersionNo <- newVersionNo[[1]]
    }
    if (length(newVersionNo) == 1) {

      if (nchar(newVersionNo) == 0) {
        newVersionNo <- NULL
      } else {
        if (length(grep(pattern = "^[[:digit:]]{1,}\\.[[:digit:]]{1,}.[[:digit:]]{1,}$",
                        x = newVersionNo,
                        fixed = FALSE)) != 1) {
          stop("Invalid version number specified. Version number must be in the format of '1.2.3'.")
        }
      }
    }
  }

  # Determine the new version's number, if none were provided.
  if (length(newVersionNo) == 0) {

    # Get the last version number from the policy config and bump it up by 0.0.1.
    polConf <- Guardener::GgetPolicy(
      refreshToken = refreshToken,
      baseurl = baseurl,
      policyId = policyId,
      assignNms = FALSE)
    els <- strsplit(x = polConf$version, split = ".", fixed = TRUE)[[1]]
    els[length(els)] <- as.numeric(els[length(els)]) + 1
    newVersionNo <- paste(els, collapse = ".")

  }

  # Get access token for this request.
  accessToken <- GgetAccessToken(
    refreshToken = refreshToken,
    baseurl = baseurl)

  # Submit the publication request to the Guardian.
  res <- httr::PUT(
    url = sprintf("%sapi/v1/policies/push/%s/publish",
                  baseurl,
                  policyId),
    httr::add_headers(
      Authorization = sprintf("Bearer %s", accessToken)),
    body = list(policyVersion = newVersionNo),
    encode = "json")

  # Process the result.
  {
    if (res$status_code < 200 | res$status_code > 299) {
      stCode <- res$status_code
      errMsg <- httr::content(res, as = "parsed")
      stop(sprintf("Failed to publish policy: %s (%s)", stCode, errMsg))
    }

    res <- httr::content(res, as = "parsed")

    # Wait for the publication to finish.
    {
      taskId <- res$taskId

      bDone <- FALSE
      bTimeOut <- FALSE # We'll time-out after 5 minutes.
      tsStart <- Sys.time()
      while (!bDone & !bTimeOut) {

        # Give the Guardian some time.
        Sys.sleep(5)

        # Get task status update.
        res <- Guardener::GgetTaskStatus(
          refreshToken = refreshToken,
          baseurl = baseurl,
          taskId = taskId)

        if ("error" %in% names(res)) {
          if (length(res$error) > 0) {
            if (length(res$error$code) > 0 || length(res$error$message) > 0) {
              stop(sprintf("Failed to publish policy: %s (%s)",
                           res$error$message, res$error$code))
            }
          }
        }

        if ("result" %in% names(res)) {

          if ("errors" %in% names(res$result)) {
            if (length(res$result$errors) > 0) {
              if ("errors" %in% names(res$result$errors)) {
                if (length(res$result$errors$errors) > 0) {
                  stop("Failed to publish policy. Errors: ",
                       paste(res$result$errors$errors, collapse = "; "))
                }
              }
            }
          }

          if ("isValid" %in% names(res$result)) {
            if (!res$result$isValid) {
              stop("Failed to publish policy. The policy is not valid.")
            }
          }

          bDone <- TRUE

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
  }

  # Return TRUE.
  return(TRUE)

}


