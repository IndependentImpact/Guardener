GgetPolicyConfig <- function(accessToken = NULL,
                             un = NULL,
                             pw = NULL,
                             baseurl = "http://localhost:3000/",
                             policyId = NULL,
                             returndf = FALSE,
                             ...){
  res <- map_df(policyId, ~{
    message(.)
    ID = .
    policy <- GgetPolicy(accessToken = AT, un = un, pw = pw, baseurl = baseurl, policyId = .)
    df <- Glist2tibble(policy$config, flatfirst = F)
    nn <- names(df)[map_lgl(df, ~length(.[[1]]) > 1)]
    df <- df %>% unnest(cols = -{{nn}})
    nn2 <- names(df)[map_lgl(df, ~length(.[[1]]) > 1)]
    df %>% unnest(cols = -{{nn2}}) %>% mutate(policyId = ID)
  })
}
