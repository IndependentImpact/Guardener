#' @title GgetTaskStatus
#' @description Returns the status of a Guardian task. Only users with the Standard
#'   Registry role are allowed to make the request.
#' @param taskId Character. The ID of the task to get the status of.
#' @param refreshToken Character. JWT refresh token returned by Glogin()$refreshToken.
#' @param baseurl Character. Base URL of the targeted Guardian instance. Defaults
#'   to "http://localhost:3000/".
#' @return A list with the task details and status.
#' @export
#'
GgetTaskStatus <- function(taskId,
                           refreshToken,
                           baseurl = "http://localhost:3000/") {

  # Get access token for this request.
  accessToken <- GgetAccessToken(
    refreshToken = refreshToken,
    baseurl = baseurl)

  # Make the request.
  res <- httr::GET(
    url = sprintf("%sapi/v1/tasks/%s", baseurl, taskId),
    httr::add_headers(
      Authorization = sprintf("Bearer %s", accessToken)))

  # Process the result.
  {
    if (res$status_code != 200) {
      stCode <- res$status_code
      errMsg <- httr::content(res, as = "parsed")
      stop(sprintf("Failed to get task status: %s (%s)", errMsg, stCode))
    }

    res <- httr::content(res, as = "parsed")

    # Transform res$statuses into a data frame.
    dfStatus <- res$statuses
    dfStatus <- lapply(X = dfStatus, FUN = data.frame)
    dfStatus <- do.call("rbind", dfStatus)
    res$statuses <- dfStatus
  }

  # Done.
  return(res)

}
