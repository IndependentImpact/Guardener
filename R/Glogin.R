
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

  if (xLogin$status_code != 200) {
    res <- httr::content(xLogin, as = "parsed")
    stop("Login failed. Status code: ", xLogin$status_code, " Message: ", res$message)
  }

  if (returnraw) return(xLogin)

  httr::content(xLogin, as = "parsed")
}
