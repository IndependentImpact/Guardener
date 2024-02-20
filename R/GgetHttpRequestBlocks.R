#' GgetHttpRequestBlocks
#' @description
#' Convenience function to get just the httpRequestBlock from a policy
#' @param refreshToken Character . A valid refreshtoken
#' @param baseurl Character. A valid Url. Defauklt "http://localhost:3000/"
#' @param policyId Character. A Guardian policy ID
#'
#' @return tibble
#' @export
#'
#' @examples
#' GgetHttpRequestBlocks("65c3cbe7320d203db7dd6e44")

GgetHttpRequestBlocks <- function(refreshToken = NULL,
                                  baseurl = "http://localhost:3000/",
                                  policyId = NULL){
        blocks <- GgetPolicyBlocks(refreshToken = refreshToken,
                                   baseurl = baseurl,
                                   policyId = policyId,
                                   returndf = TRUE,
                                   schemaBlocksOnly = FALSE)
        blocks %>% filter(blockType == "httpRequestBlock")
}
