#' GgetSchemas
#' @description Get schemas based on a topicID
#' @param refreshToken Character. JWT refresh token returned by Glogin()$refreshToken.
#' @param baseurl Character. Base url. defaults to "http://localhost:3000/"
#' @param pageIndex Integer. default 0
#' @param pageSize Integer. default 10
#' @param topicId Character
#' @import dplyr
#' @import purrr
#' @import httr
#' @return tibble or list
#' @export

GgetSchemas <- function(refreshToken,
                        baseurl = "http://localhost:3000/",
                        pageIndex = 0,
                        pageSize = 10,
                        topicId = NULL,
                        schemaId = NULL,
                        returndf = FALSE){

  # Pass on to GgetSchema() if the user actually just wants a specific schema.
  if (!is.null(schemaId)){
    return(GgetSchema(refreshToken = refreshToken,
                      baseurl = baseurl,
                      schemaId = schemaId,
                      returndf = returndf))
  }

  # Build url.
  if (!is.null(topicId)) {
    message("Using TopicId ", topicId)
    url = sprintf("%sapi/v1/schemas/%s", baseurl, topicId)
  } else {
    message("No schemaId or TopicId. Getting everything")
    url = sprintf("%sapi/v1/schemas", baseurl)
  }

  # Get access token for this query.
  accessToken <- Guardener::GgetAccessToken(
    refreshToken = refreshToken,
    baseurl = baseurl)

  # Make the query.
  res <- httr::GET(url = url,
                   pageIndex = pageIndex,
                   pageSize = pageSize,
                   httr::add_headers(Authorization = sprintf("Bearer %s", accessToken)))

  if (res$status_code != 200) {
    stop(sprintf("Error: Query returned status code %s.", res$status_code))
  }

  res <- httr::content(res)

  # Assign schema IRIs as list names
  names(res) <- sapply(X = res, FUN = function(x) x$iri)

  if (returndf){
    df <- Guardener::Gschema2tibble(res)
    return(df)
  }

  return(res)
}
