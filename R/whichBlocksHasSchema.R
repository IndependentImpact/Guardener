

#' whichBlocksHasSchema
#' @description Check in a collection of blocks which blocks are associated with a particular schema
#' @param dfBlocks tibble. Typically the output of GgetPolicyBlocks()
#' @param schemaId character. The uuid of a schema
#' @param washfunction function or NULL. Function to clean the variable containing the schemaId (specified by schemaname)
#' in dfBlocks. Default function(x){gsub("#", "", x)} . If NULL no cleaing is performed
#' @param schemaname Character. Variable in dfBlocks in which to look for schemaId
#' @return tibble
#' @export
#'
#' @examples b <- whichBlocksHasSchema(GgetPolicyBlocks(AT, policyId = policyid),  "a399cc58-9a79-4277-b5ef-1c6461843a58")

whichBlocksHasSchema <- function(dfBlocks,
                                 schemaId,
                                 washfunction = c(function(x){gsub("#", "", x)}, NULL)[1],
                                 schemaname = "schema"
                                 ){
  message(schemaId)
  if (!is.data.frame(dfBlocks)) stop("dfBlocks must be a data frame")
  if (!schemaname %in% colnames(dfBlocks)) stop(schemaname , " does not occur in dfBlocks")
  if (!is.null(washfunction)) {dfBlocks <- dfBlocks %>% mutate(schema = washfunction[[1]](.[[!!schemaname]]))}

  dfBlocks %>% filter(.[[!!schemaname]] %in% schemaId)

}
