#' extractTopic
#' Convenience function to extract topicId from policy
#' @param policy
#'
#' @return
#'
#' @examples
#' extractTopic(policy)

extractTopic <- function(policy){
  topicId <- policy$topicId
}

#' ones
#' Convenience function to extract names of columns that contain nested items of length 1
#' @param df
#'
#' @return character vector with names of df that contain nested items of length 1
#' @export
#'
#' @examples df %>% unnest(cols = ones(.))

ones <- function(df){
  names(df)[map_lgl(df, ~all(map_int(., length) == 1))]
}
