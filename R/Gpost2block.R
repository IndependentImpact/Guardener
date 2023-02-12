
#' Gpost2Schema
#'
#' @param accessToken character. Access token. typically from 'accessToken' field in result of Glogin()
#' @param document Named list or data frame with data to submit. Names must correspond to field names in Schema
#' @param schemaID character. Schema to submit to
#' @param schemaIPFSurl character. Schema to submit to
#' @param policyId character. Schema to submit to
#' @param BlockId character. Schema to submit to
#' @param baseurl Character. URL where /api/ is hosted. Default "http://localhost:3000/"
#'
#' @return
#' @export

Gpost2block <- function(accessToken = NULL,
                         document = NULL,
                         schemaID = NULL,
                         schemaIPFSurl = NULL,
                         policyId = NULL,
                         BlockId = NULL,
                         baseurl = "http://localhost:3000/"){
  if (map_lgl(list(accessToken , document , schemaID, schemaIPFSurl , policyId, BlockId ), ~all(is.null(.)|is.na(.))) %>% any()) stop("None of ccessToken , document , schemaID, schemaIPFSurl , policyIdor BlockId may be NULL")
  httr::POST(
    url = sprintf("%sapi/v1/policies/%s/blocks/%s", baseurl, policyId, BlockId),
    httr::add_headers(Authorization = sprintf("Bearer %s", accessToken)),
    body = GmakeBody(document, schemaID, schemaIPFSurl),
    encode = "json")
}

