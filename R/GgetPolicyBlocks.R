
#' GgetPolicyBlocks
#'
#' @param accessToken Character. Access token returned by Glogin()  $accessToken
#' @param un Character. Username
#' @param pw Character. Password
#' @param baseurl Character. Base url. defaults to "http://localhost:3000/"
#' @param policyId Character. Typically from the $id item of the result of GgetPolicies()
#' @return tibble
#' @examples
#' @export
#'
GgetPolicyBlocks <- function(accessToken = NULL,
                             un = NULL,
                             pw = NULL,
                             baseurl = "http://localhost:3000/",
                             policyId = NULL,
                             returndf = FALSE){
  dfConfig <- GgetPolicyConfig(accessToken = AT, un = un, pw = pw,
                               baseurl = baseurl, policyId = policyId)

  tibble(
    value = unname(unlist(dfConfig$children[[1]])),
    name = unlist(dfConfig$children[[1]]) %>% names()
    )  %>%
    mutate(label = gsub("([[:print:]]*)(\\.)([[:alnum:]]+$)",  "\\3", name),
           name.rest = gsub("([[:print:]]*)(\\.)([[:alnum:]]+$)",  "\\1", name)) %>%
    select(name.rest, label, value) %>%
    group_by(name.rest) %>%
    nest() %>%
    mutate(hasSchema = map_lgl(data, ~"schema" %in% unlist(.[1]))) %>%
    filter(hasSchema) %>%
    select(-hasSchema) %>%
    mutate(data = map_df(data, ~pivot_wider(., names_from = label, values_from = value))) %>%
    unnest(cols = c(data)) %>%
    ungroup() %>%
    select(-name.rest) %>%
    select(tag, everything())
}

