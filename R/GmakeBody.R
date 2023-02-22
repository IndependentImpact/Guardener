
#' GmakeBody
#'
#' @param document Named list or data frame with data to submit. Names must correspond to field names in Schema
#' @param schemaID Character
#' @param schemaIPFSurl Character
#'
#' @return
#' @export

GmakeBody <- function(document = NULL, schemaID = NULL, schemaIPFSurl = NULL){

  if ( is.data.frame(document)) document <- as.list(document)
  if (!is.list(document)) stop("document must be a list")

  append(document,
        list(
          type = gsub(pattern = "^#{1}",
                      replacement = "", x = schemaID),
          '@context' = schemaIPFSurl)
        )
}


