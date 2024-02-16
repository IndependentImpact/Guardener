

resLogin <- Glogin(
  un = "StandardRegistry",
  pw = "test",
  baseurl = "http://167.99.35.174:3000/")
refreshToken <- resLogin$refreshToken
messageId <- "1707337505.219810903"
fp <- "C:/Users/ALIIIX/Documents/DraftsEnStuff/Improved Cookstove Policy (API-only) - v5.1.0.zip"




GmakePolicyCopy <- function(refreshToken,
                            baseurl = "http://localhost:3000/",
                            messageId = NULL,
                            fp = NULL,
                            publish = TRUE) {

  # Input checking.
  if (length(messageId) == 0 & length(fp) == 0) {
    stop("Either 'messageId' or 'fp' must be provided.")
  }

  # Make the copy request.
  {
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
      taskId <- res$taskId

      res <- Guardener::GgetTaskStatus(
        refreshToken = refreshToken,
        baseurl = baseurl,
        taskId = taskId)

      # TODO.
    }

    # Get the policyId of the new copy.
    policyId <- res$result$policyId
  }



  # If the user does not want to publish the new policy, we're done, so just
  # return the policyId.
  if (!publish) {
    return(res2$result$policyId)
  }




  # Change the policy's name.
  # (At this point the name will have an ugly tag behind it, e.g.,
  # "ICP - Agent Application Subpolicy (API-only)_1708102794423")
  {
    # Get the policy's config.
    polConf <- Guardener::GgetPolicy(
      refreshToken = refreshToken,
      baseurl = baseurl,
      policyId = res2$result$policyId)

    # Change the name.
    polConf$name <- sub("_.*$", "", polConf$name)

    # Get access token for next request.
    accessToken <- GgetAccessToken(
      refreshToken = refreshToken,
      baseurl = baseurl)

    # Push name change to Guardian instance.
    res <- httr::PUT(
      url = sprintf("%sapi/v1/policies/%s", baseurl, res2$result$policyId),
      httr::add_headers(
        Authorization = sprintf("Bearer %s", accessToken)),
      httr::content_type("application/json"),
      encode = "raw",
      body = jsonlite::toJSON(
        x = polConf,
        #dataframe = , #matrix = , #Date = , #POSIXt = ,
        factor = "string",
        complex = "string",
        #raw = , #null = , #na = ,
        auto_unbox = TRUE, # The request fails when auto_unbox = FALSE.
        #digits = ,
        pretty = FALSE,
        force = FALSE))

    # Process the result.
    if (res$status_code != 200) {
      stCode <- res$status_code
      errMsg <- httr::content(res, as = "parsed")
      stop(sprintf("Failed to change policy name: %s (%s)",
                   errMsg, stCode))
    }
  }


  # Make the publication request.
  {
    # Get access token for this request.
    accessToken <- GgetAccessToken(
      refreshToken = refreshToken,
      baseurl = baseurl)

    # Make the actual request.
    # TODO. Determine the policy version.
    res <- httr::PUT(
      url = sprintf("%sapi/v1/policies/push/%s/publish",
                    baseurl,
                    res2$result$policyId),
      httr::add_headers(
        Authorization = sprintf("Bearer %s", accessToken)),
      body = list(policyVersion = "1.0.0")) # TODO.

    # Process the result.
    {
      if (res$status_code != 202) {
        stCode <- res$status_code
        errMsg <- httr::content(res, as = "parsed")
        stop(sprintf("Failed to publish policy: %s (%s)",
                     stCode, errMsg))
      }

      res <- httr::content(res, as = "parsed")

      # Wait for the publication to finish.
      {
        taskId <- res$taskId

        res <- Guardener::GgetTaskStatus(
          refreshToken = refreshToken,
          baseurl = baseurl,
          taskId = taskId)

        # TODO.
      }
    }
  }

  # Return the policyId.
  return(policyId)

}


