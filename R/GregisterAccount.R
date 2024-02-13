
#' @title GregisterAccount
#' @description Creates a new Guardian user account on the Guardian instance at {baseurl}.
#'    Note that this function does not associate the new user with any specific
#'    standards registry on the Guardian instance in question; the association
#'    with a standards registry must be done separately.
#' @param baseurl Character. URL where /api/ is hosted. Default "http://localhost:3000/".
#' @param un Character. The username to be registered.
#' @param pw Character. The password to be registered.
#' @param role Character. The Guardian user type to be registered. Defaults to "USER".
#' @return A list with at least the following elements:
#'  - id: The user's unique Guardian identifier (e.g., "65ca39498ae968bb194096be")
#'  - username: The user's username (e.g., "jdoe")
#'  - role: The user's role (e.g., "STANDARD_REGISTRY or "USER")
#' @export
#'
GregisterAccount <- function(baseurl,
                             un,
                             pw,
                             role = "USER") {

  # Make the query.
  res <- httr::POST(url = sprintf("%sapi/v1/accounts/register", baseurl),
              body = list(username = un,
                          password = pw,
                          password_confirmation = pw,
                          role = role))

  if (res$status_code != 201) {
    stop(sprintf("Failed to create an account for user '%s' on Guardian instance at '%s'. Status code: %s",
                 un, baseurl, res$status_code))
  }

  # Process the result.
  res <- httr::content(res, as = "parsed")

  # Done.
  return(res)
}
