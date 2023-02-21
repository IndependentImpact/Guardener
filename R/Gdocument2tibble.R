
#' Gdocument2tibble
#'
#' @param tibble
#'
#' @return tibble
#' @export
#'
#' @examples

Gdocument2tibble <- function(d){
  bd <- jsonlite::fromJSON(d)
  df <- tibble(bd) %>%
    pivot_longer(everything()) %>%
    mutate(name = names(bd))

  nn <- df$name[map_lgl(1:nrow(df), ~length(df$value[[.]]) > 1 | class(df$value[[.]]) == "list")]

  df <- df %>%
    pivot_wider(names_from = name)

  df <- df %>% unnest(cols = -{{nn}})
  df
}
