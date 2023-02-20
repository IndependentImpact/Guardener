
#' Glogin
#'
#' @param un Character. Username
#' @param pw Character. Password
#' @param baseurl Character. Base url. defaults to "http://localhost:3000/"
#'
#' @return list with "username", "did", "role" and "accessToken"
#' @examples
#' @export
#'
Glogin <- function(un = Sys.getenv("guardianUn"),
                   pw = Sys.getenv("guardianPW"),
                   baseurl = "http://localhost:3000/"){
  xLogin <- httr::POST(url = sprintf("%sapi/v1/accounts/login", baseurl),
       body = list(username = un, password = pw))
  httr::content(xLogin)
}


