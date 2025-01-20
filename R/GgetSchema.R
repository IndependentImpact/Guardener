#' GgetSchema
#' @description Get a specific schema.
#' @param refreshToken Character. JWT refresh token returned by Glogin()$refreshToken.
#' @param baseurl Character. Base url of the targeted Guardian instance.
#'    Defaults to "http://localhost:3000/".
#' @param schemaId Character. The ID of the schema to retrieve.
#' @import httr
#' @return list or data frame
#' @export

GgetSchema <- function(refreshToken,
                       baseurl = "http://localhost:3000/",
                       schemaId,
                       returndf = FALSE){

  # Get access token for this query.
  accessToken <- GgetAccessToken(refreshToken = refreshToken,
                                 baseurl = baseurl)

  # Make the query.
  url <- sprintf("%sapi/v1/schema/%s", baseurl, gsub("#","", schemaId))
  res <- httr::GET(url = url,
                   httr::add_headers(Authorization = sprintf("Bearer %s", accessToken)))

  if (res$status_code != 200) {
    stop(sprintf("Error: Query returned status code %s.", res$status_code))
  }

  # Process the result.
  res <- httr::content(res)

  # Due to a bug in the Guardian API, the '$defs' field is empty in the version of the
  # schema definition included in the 'document' list item, so we need to replace that
  # item with the full definition retrieved from IPFS.
  ipfsUrl <- res$documentURL
  if (length(grep(pattern = "http", x = ipfsUrl, value = FALSE)) == 0) {
    ipfsUrl <- gsub(pattern = "ipfs://", replacement = "", x = ipfsUrl, fixed = TRUE)
    ipfsUrl <- sprintf("https://%s.ipfs.w3s.link", ipfsUrl)
  }
  doc <- httr::GET(url = ipfsUrl)
  doc <- content(x = doc, as = "parsed")
  res$document <- jsonlite::toJSON(x = doc, auto_unbox = TRUE)

  if (returndf){
    res <- res[which(sapply(X = res, FUN = function(x) length(x) > 0))]
    res <- data.frame(res)
  }

  return(res)
}
