#' Title
#'
#' @param accessToken Character. Access token returned by Glogin()  $accessToken
#' @param un Character. Username
#' @param pw haracter. Password
#' @param baseurl Character. Base url. defaults to "http://localhost:3000/"
#' @param policyId haracter. Typically from the $id item of the result of GgetPolicies()
#' @param returndf Return a tibble or not Default FALSE.
#' @param ...
#'
#' @return
#' @export

GgetPolicyConfig <- function(accessToken = NULL,
                             un = NULL,
                             pw = NULL,
                             baseurl = "http://localhost:3000/",
                             policyId = NULL,
                             returndf = FALSE,
                             verbose = FALSE,
                             ...){
  res <- map_df(policyId, ~{
    if (verbose) message(.)
    ID = .
    policy <- GgetPolicy(accessToken = AT, un = un, pw = pw, baseurl = baseurl, policyId = .)
    df <- Glist2tibble(policy$config, flatfirst = F)
    nn <- names(df)[map_lgl(df, ~length(.[[1]]) > 1)]
    df <- df %>% unnest(cols = -{{nn}})
    nn2 <- names(df)[map_lgl(df, ~length(.[[1]]) > 1)]
    df %>% unnest(cols = -{{nn2}}) %>% mutate(policyId = ID)
  })
}
