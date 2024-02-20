#' GgetPolicyBlocks
#'
#' @param refreshToken Character. JWT refresh token returned by Glogin()$refreshToken.
#' @param baseurl Character. Base url. defaults to "http://localhost:3000/"
#' @param policyId Character. Typically from the $id item of the result of GgetPolicies()
#' @param schemaBlocksOnly Logical. If TRUE, only return blocks that have a schema.
#' @return tibble
#' @export

GgetPolicyBlocks <- function(refreshToken = NULL,
                             baseurl = "http://localhost:3000/",
                             policyId = NULL,
                             returndf = FALSE,
                             schemaBlocksOnly = TRUE){

  dfConfig <- GgetPolicyConfig(refreshToken = refreshToken,
                               baseurl = baseurl,
                               policyId = policyId)

  res <- purrr::map_df(
    1:nrow(dfConfig),
    ~{
      tibble(policyId = dfConfig$policyId[[.]],
             value = unname(unlist(dfConfig$children[[.]])),
             name = unlist(dfConfig$children[[.]]) %>% names())
    }) %>%
    mutate(label = gsub("([[:print:]]*)(\\.)([[:alnum:]]+$)",  "\\3", name),
           name.rest = gsub("([[:print:]]*)(\\.)([[:alnum:]]+$)",  "\\1", name)) %>%
    select(policyId, name.rest, label, value) %>%
    group_by(policyId, name.rest) %>%
    tidyr::nest() %>%
    mutate(hasSchema = purrr::map_lgl(data, ~"schema" %in% unlist(.[1])))

  if (schemaBlocksOnly) {
    res <- res %>%
      filter(hasSchema) %>%
      select(-hasSchema)
    if (nrow(res) == 0) {
      return(NULL)
    }
  }

  res <- res %>%
    filter(!grepl("uiMetaData", name.rest)) %>%
    mutate(data = purrr::map_df(data, ~tidyr::pivot_wider(., names_from = label, values_from = value))) %>%
    tidyr::unnest(cols = c(data)) %>%
    ungroup() %>%
    select(-name.rest) %>%
    select(policyId, tag, everything())

  return(res)
}
