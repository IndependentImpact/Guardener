
#' Title
#'
#' @param refreshToken Character. JWT refresh token returned by Glogin()$refreshToken.
#' @param baseurl Character. Base url. defaults to "http://localhost:3000/"
#' @param policyId haracter. Typically from the $id item of the result of GgetPolicies()
#' @param returndf Return a tibble or not Default FALSE.
#' @param ...
#'
#' @return
#' @export

GgetPolicyConfig <- function(refreshToken = NULL,
                             baseurl = "http://localhost:3000/",
                             policyId = NULL,
                             returndf = FALSE,
                             verbose = FALSE,
                             ...){

  # Get access token for this query.
  accessToken <- GgetAccessToken(refreshToken = refreshToken,
                                 baseurl = baseurl)

  # Make the query for each policyId.
  res <- purrr::map_df(policyId, ~{
    if (verbose) message(.)
    ID = .
    policy <- GgetPolicy(refreshToken = refreshToken,
                         baseurl = baseurl,
                         policyId = .)
    df <- Glist2tibble(policy$config)
    nn <- names(df)[purrr::map_lgl(df, ~length(.[[1]]) > 1)]
    df <- df %>% tidyr::unnest(cols = -{{nn}})
    nn2 <- names(df)[purrr::map_lgl(df, ~length(.[[1]]) > 1)]
    df %>% tidyr::unnest(cols = -{{nn2}}) %>% mutate(policyId = ID)
  })

  return(res)
}
