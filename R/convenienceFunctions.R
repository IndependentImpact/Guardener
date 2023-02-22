
extractTopic <- function(policy){
  topicId <- policy$topicId
}

#' ones
#'
#' @param df
#'
#' @return character vector with names of df that contain nested items of length 1
#' @export
#'
#' @examples df %>% unnest(cols = ones(.))

ones <- function(df){
  names(df)[map_lgl(df, ~all(map_int(., length) == 1))]
}
