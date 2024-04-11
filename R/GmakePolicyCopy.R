#' @title GmakePolicyCopy
#' @description Makes a copy of a Guardian policy and pushes it back to the
#'    Guardian instance. Can optionally change the name of the copy and publish it.
#' @param refreshToken Character. JWT refresh token returned by Glogin()$refreshToken.
#' @param messageId Character. The message ID of the policy to be copied. If provided,
#'    the function will ignore the 'fp' argument. If not provided, the function will
#'    try to copy the policy from the ZIP file provided in the 'fp' argument.
#' @param fp Character. The file path of the ZIP file of the policy to be copied.
#'    Ignored if 'messageId' is provided.
#' @param newName Character. Optional. The name to give the new copy of the policy.
#' @param newVersionNo Character. Optional. The new version number of the policy. If
#'  not provided and publish = TRUE, the function will try to determine the new
#'  version number automatically. It is strongly recommended to provide the correct
#'  version number yourself. This argument is ignored if publish = FALSE.
#' @param publish Logical. Whether to publish the new policy. Defaults to TRUE.
#' @param baseurl Character. Base URL of the targeted Guardian instance. Defaults
#'   to "http://localhost:3000/".
#' @return Character. The Guardian policy ID of the newly created copy.
#' @export
#'
GmakePolicyCopy <- function(refreshToken,
                            messageId = NULL,
                            fp = NULL,
                            newName = NULL,
                            newVersionNo = NULL,
                            publish = TRUE,
                            baseurl = "http://localhost:3000/") {

  # Input checking.
  {
    # messageId vs. fp
    {
      if (length(messageId) == 0 & length(fp) == 0) {
        stop("Either 'messageId' or 'fp' must be provided.")
      }
      if (length(messageId) == 1 & length(fp) == 1) {
        stop("Both 'messageId' and 'fp' provided; 'fp' will be ignored.")
      }
    }

    # newName
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

    # newVersionNo
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
  }

  # Initialise some variables.
  policyId <- NULL
  polConf <- NULL

  # Make the copy request.
  {
    message("Starting copy task...")

    # Get access token for this request.
    accessToken <- GgetAccessToken(
      refreshToken = refreshToken,
      baseurl = baseurl)

    # Make the actual request.
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
      message("Waiting for copy task to finish...")

      taskId <- res$taskId

      bDone <- FALSE
      while (!bDone) {

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
      }
    }

    # Check for unknown errors.
    {
      if (length(policyId) == 0) {
        stop("No policyId returned, but no errors returned either.")
      }
    }

  }

  # Try to change the policy's name, if the user provided a new name.
  # (At this point the name will have an ugly tag behind it, e.g.,
  # "ICP - Agent Application Subpolicy (API-only)_1708102794423")
  if (length(newName) == 1) {

    message("Renaming policy copy...")

    tryCatch({

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

    }, error = function(e) {
      warning(sprintf("Failed to change policy name: %s", e$message))
    })
  }

  # If the user does not want to publish the new policy, we're done, so just
  # return the policyId.
  if (!publish) {
    return(policyId)
  }

  # Determine the new version's number, if none were provided.
  if (length(newVersionNo) == 0) {
    if (length(newName) == 1) {
      newVersionNo <- "1.0.0"
    } else {

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
  }

  # Make the publication request.
  {
    message("Starting policy publication task...")

    # Get access token for this request.
    accessToken <- GgetAccessToken(
      refreshToken = refreshToken,
      baseurl = baseurl)

    # Make the actual request.
    res <- httr::PUT(
      url = sprintf("%sapi/v1/policies/push/%s/publish",
                    baseurl,
                    policyId),
      httr::add_headers(
        Authorization = sprintf("Bearer %s", accessToken)),
      body = list(policyVersion = newVersionNo))

    # Process the result.
    {
      if (res$status_code != 202) {
        stCode <- res$status_code
        errMsg <- httr::content(res, as = "parsed")
        stop(sprintf("Failed to publish policy: %s (%s)", stCode, errMsg))
      }

      res <- httr::content(res, as = "parsed")

      # Wait for the publication to finish.
      {
        message("Waiting for policy publication task to finish...")

        taskId <- res$taskId

        bDone <- FALSE
        while (!bDone) {

          res <- GgetTaskStatus( #Guardener::
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

        }
      }
    }
  }

  # Return the policyId.
  return(policyId)

}


