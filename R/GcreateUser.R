
#' @title GcreateUser
#' @description Convenience wrapper around the GregisterAccount, Glogin and
#'    GsetUpProfile functions which should almost always be called in succession.
#' @param un Character. The username to be registered.
#' @param pw Character. The password to be registered.
#' @param role Character. The Guardian user type to be registered. Defaults to "USER".
#' @param parentDid Character. The DID of the standard registry user to associate the user with.
#' @param hederaAccountId Character. The Hedera account ID to be associated with the user.
#' @param hederaAccountKey Character. The Hedera account key to be associated with the user.
#' @param baseurl Character. URL where /api/ is hosted. Default "http://localhost:3000/".
#' @return A list with at least the following elements:
#'  - id: The user's unique Guardian identifier (e.g., "65ca39498ae968bb194096be")
#'  - username: The user's Guardian username (e.g., "jdoe")
#'  - hash_password: The user's hashed Guardian password (e.g., "65ca39498ae968bb194096be")
#'  - role: The user's role (e.g., "STANDARD_REGISTRY or "USER")
#'  - refreshToken: The user's JWT refresh token.
#'  - did: The user's DID.
#'  - didDocMessageId: The message ID of the user's DID document.
#' @example
#'    res <- GcreateUser(
#'      un = "abcdef@email.com",
#'      pw = "abcdef12345#!#",
#'      role = "USER",
#'      parentDid = "did:hedera:testnet:G53WPzkeuisRgsC3oooikWcGzkMT6ydAgQQNEAY5jiA2_0.0.2950818",
#'      hederaAccountId = "0.0.3424007",
#'      hederaAccountKey = "302e020100300506032b657004220420ecc6dd4fffe25f7a0cbc49e4ad11b0644f4cf1c8e3ef78013db365c525e72aa9",
#'      baseurl = "http://167.99.35.174:3000/")
#' @export
#'
GcreateUser <- function(un,
                        pw,
                        role = "USER",
                        parentDid,
                        hederaAccountId,
                        hederaAccountKey,
                        baseurl = "http://localhost:3000/") {

  lsReturn <- list()

  # 1. Register new Guardian user.
  {
    res <- Guardener::GregisterAccount(
      baseurl = baseurl,
      un = un,
      pw = pw,
      role = role)

    lsReturn$username <- res$username
    lsReturn$hash_password <- res$password
    lsReturn$role <- res$role
    lsReturn$id <- res$id
  }

  # 2. Log new Guardian user in to the Guardian instance.
  {
    res <- Guardener::Glogin(
      un = un,
      pw = pw,
      baseurl = baseurl)

    lsReturn$refreshToken <- res$refreshToken
  }

  # 3. Associate this user's Hedera credentials with their Guardian account.
  {
    Guardener::GsetUpProfile(
      refreshToken = lsReturn$refreshToken,
      un = un,
      role = role,
      parentDid = parentDid,
      hederaAccountId = hederaAccountId,
      hederaAccountKey = hederaAccountKey,
      baseurl = baseurl)
  }

  # 4. Get the user's newly created DID etc..
  {
    res <- Guardener::GgetUserProfile(
      refreshToken = lsReturn$refreshToken,
      un = un,
      baseurl = baseurl)

    lsReturn$did <- res$did
    lsReturn$didDocMessageId <- res$didDocMessageId
  }

  # Done.
  return(lsReturn)
}









