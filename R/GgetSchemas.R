
#' GgetSchemas
#' @description Get schemas based on a topicID
#' @param accessToken  Character. Access token returned by Glogin()  $accessToken
#' @param baseurl Character. Base url. defaults to "http://localhost:3000/"
#' @param un Character. Username
#' @param pw haracter. Password
#' @param pageIndex Integer. default 0
#' @param pageSize Integer. default 10
#' @param topicId Character
#' @import dplyr
#' @import purrr
#' @import httr
#' @return tibble or list
#' @export

GgetSchemas <- function(accessToken = NULL, baseurl = "http://localhost:3000/",
                        un = NULL,
                        pw = NULL,
                        pageIndex = 0,
                        pageSize = 10,
                        topicId = NULL,
                        schemaId = NULL,
                        returndf = FALSE){

  if (!is.null(topicId)){
    message("Using TopicId ", topicId)
    url = sprintf("%sapi/v1/schemas/%s", baseurl, topicId)
     } else {
    if (!is.null(schemaId)){
      message("Using schemaId ", gsub("#","", schemaId))
      url = sprintf("%sapi/v1/schema/%s", baseurl, gsub("#","", schemaId))
    } else {
      message("No schemaId or TopicId. Getting everything")
      url = sprintf("%sapi/v1/schemas", baseurl)}
    }

 schemas <- httr::GET(url = url,
                pageIndex = pageIndex,
                pageSize = pageSize,
                httr::add_headers(Authorization = sprintf("Bearer %s", accessToken)))

  res <- httr::content(schemas)

  # Assign schema IRIs as list names
  names(res) <- sapply(X = res, FUN = function(x) x$iri)

  if (returndf){
    df <- Gschema2tibble(res)
    return(df)
  }

  res
}
