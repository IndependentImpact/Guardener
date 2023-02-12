
#' names2tibble
#'
#' @param data a dataframe with a column with fieldnames
#' @param fieldnames Character. names of columns to be created
#'
#' @return
#' @export

names2tibble <- function(data, fieldnames = "fieldnames"){
  data <- distinct(data)
  n <- nrow(data)
  nn <- pull(data[ ,fieldnames])
  dup <- duplicated(nn)
  if (any(dup)) message("!!!!!!!!!!\n ", paste(nn[dup], " "), "\n-----------")

 # message("\n", paste(nn, " ") , "n")

  data.frame(t(rep(NA_character_, n))) %>%
    set_names(nn) %>%
    tibble()
}
