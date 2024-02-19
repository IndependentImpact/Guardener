# library(Guardener)
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
# dfUsers <- GgetAccounts(
#   refreshToken = resLogin$refreshToken,
#   baseurl = baseUrl)
#
# dfUserProf <- GgetUserProfile(
#   refreshToken = resLogin$refreshToken,
#   username = "StandardRegistry",
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
#
# dfSchema <- GgetSchema(
#   refreshToken = resLogin$refreshToken,
#   baseurl = baseUrl,
#   schemaId = "65b0d3952b3252fcc5b23a85",
#   returndf = TRUE)
#
# dfTokens <- GgetTokens(
#   refreshToken = resLogin$refreshToken,
#   baseurl = baseUrl)
#
# dfTokenPols <- GgetTokenPolicyIds(
#   refreshToken = resLogin$refreshToken,
#   baseurl = baseUrl)
#
# dfUserTknInfo <- GgetUserTokenInfo(
#   refreshToken = resLogin$refreshToken,
#   tokenId = "0.0.7717332",
#   usernm = "Installer",
#   baseurl = baseUrl)
#
# baseUrl <- basurl <- "http://167.99.35.174:3000/"
# resLogin <- Glogin(
#   un = "StandardRegistry",
#   pw = "test",
#   baseurl = baseUrl)
# refreshToken <- resLogin$refreshToken
# messageId <- "1707337505.219810903"
# fp <- "C:/Users/ALIIIX/Documents/Nova/PROJECTS/AARTUM/Hedera/AlexNotes/AAP.policy"

