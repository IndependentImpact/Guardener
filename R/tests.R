#
# baseUrl <- "http://localhost:3000/"
#
# resLogin <- Glogin(un = "StandardRegistry",
#                    pw = "test",
#                    baseurl = baseUrl)
#
# dfStandardsRs <- GgetStandardsRegistries(
#   refreshToken = resLogin$refreshToken,
#   baseurl = baseUrl)
#
# dfPolicies <- GgetPolicies(
#   refreshToken = resLogin$refreshToken,
#   baseurl = baseUrl,
#   returndf = TRUE)
#
# dfPolicy <- GgetPolicy(
#   refreshToken = resLogin$refreshToken,
#   baseurl = baseUrl,
#   returndf = TRUE,
#   policyId = dfPolicies$id[1])
#
# dfBlocks <- GgetPolicyBlocks(
#   refreshToken = resLogin$refreshToken,
#   baseurl = baseUrl,
#   policyId = dfPolicies$id[1],
#   returndf = TRUE)
#
# dfConfig <- GgetPolicyConfig(
#   refreshToken = resLogin$refreshToken,
#   baseurl = baseUrl,
#   policyId = dfPolicies$id[1],
#   returndf = TRUE,
#   verbose = FALSE)
#
# dfSchemas <- GgetSchemas(
#   refreshToken = resLogin$refreshToken,
#   baseurl = baseUrl,
#   topicId = dfPolicies$topicId[1],
#   returndf = TRUE)
