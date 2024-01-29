#' GgetPolicies
#' @description Get the policies associated with an user identified by an access token or username and passwork
#' @param refreshToken Character. JWT refresh token returned by Glogin()$refreshToken.
#' @param baseurl Character. Base url. defaults to "http://localhost:3000/"
#' @param pageIndex Integer. default 0
#' @param pageSize Integer. default 10
#' @param returndf Logical. Returns a tibble if TRUE. Else a list (default)
#' @return tibble or list
#' @export

GgetPolicies <- function(refreshToken,
                         baseurl = "http://localhost:3000/",
                         pageIndex = 0,
                         pageSize = 10,
                         returndf = FALSE,
                         verbose = FALSE){

  # Get access token for this query.
  accessToken <- GgetAccessToken(refreshToken = refreshToken,
                                 baseurl = baseurl)

  # Make the query.
  res <- httr::GET(url = sprintf("%sapi/v1/policies", baseurl),
                   pageIndex = pageIndex,
                   pageSize = pageSize,
                   httr::add_headers(Authorization = sprintf("Bearer %s", accessToken)))

  if (res$status_code != 200) {
    stop(sprintf("Error: Query returned status code %s.", res$status_code))
  }

  res <- httr::content(res)

  if (returndf) {
    df <- Glist2tibble(res, verbose = verbose)
    return(df)
  }

  return(res)
}
