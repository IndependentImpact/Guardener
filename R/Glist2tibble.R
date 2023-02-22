

#' Glist2tibble
#'
#' @param L list
#'
#' @return tibble
#' @export

Glist2tibble <- function(L, verbose = FALSE){
  if (any(map_lgl(L, ~is.null(.))))   L[map_lgl(L, ~is.null(.))] <- NA_character_
  if (any(map_lgl(L, ~length(.)==0))) L[map_lgl(L, ~length(.)==0)]  <- NA_character_

  if (verbose) assign("LL", L, envir = .GlobalEnv)

  if (length(L) == 1) L <- L[[1]]

  if (is.null(names(L))) {
    df <- map_df(L, ~{
      n <- names(.)
      as_tibble_col(.) %>%
        mutate(names = n) %>%
        pivot_wider(names_from = "names")}
    )
  } else {
    df <- as_tibble_col(L) %>%
      mutate(names = names(L)) %>%
      pivot_wider(names_from = "names")
  }

  if (verbose) assign("dff", df, envir = .GlobalEnv)

  nn <- names(df)[map_lgl(df, ~length(.[[1]]) > 1| is.null(.[[1]]) | length(.[[1]]) == 0 )]
  if (length(nn) == 0) {
    return(df)
  }
  df %>% unnest(cols = -{{nn}})

}
