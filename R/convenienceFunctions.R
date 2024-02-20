
#' @title extractTopic
#' @description Convenience function to extract topicId from policy.
#' @param policy Character.
#' @example
#'    extractTopic(policy)
#' @export

extractTopic <- function(policy){
  topicId <- policy$topicId
}

#' @title ones
#' @description Convenience function to extract names of columns that contain nested items of length 1.
#' @return Character vector with names of df that contain nested items of length 1.
#' @export
#'
ones <- function(df){
  names(df)[purrr::map_lgl(df, ~all(purrr::map_int(., length) == 1))]
}

#df %>% unnest(cols = ones(.))
