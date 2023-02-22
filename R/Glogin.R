
#' Glogin
#'
#' @param un Character. Username
#' @param pw Character. Password
#' @param baseurl Character. Base url. defaults to "http://localhost:3000/"
#' @param returnraw logical. Return the complete result before extraxting the content
#'
#' @return list with "username", "did", "role" and "accessToken"
#' @export

Glogin <- function(un = Sys.getenv("guardianUn"),
                   pw = Sys.getenv("guardianPW"),
                   baseurl = "http://localhost:3000/",
                   returnraw = FALSE){
  xLogin <- httr::POST(url = sprintf("%sapi/v1/accounts/login", baseurl),
       body = list(username = un, password = pw))
  if (returnraw) return(xLogin)

  httr::content(xLogin)
}
