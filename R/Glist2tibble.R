#' Glist2tibble
#'
#' @param L list
#' @param flatfirst logical
#' @return tibble
#' @export

Glist2tibble <- function(L, flatfirst = FALSE){
  if (flatfirst){
    df <- map_df(L, ~{
    Lnames <- flatten(.)  %>% names()
    Lf <- flatten(.)
    Lf[map_lgl(Lf, ~is.null(.))] <- NA_character_
    Lf[map_lgl(Lf, ~length(.)==0)]  <- NA_character_
    names(Lf) <- Lnames
    df <- Lf %>%
      tibble() %>%
      pivot_longer(everything()) %>%
      mutate(name = Lnames) %>%
      #unnest(value) %>%
      pivot_wider(names_from = name, values_fn = NULL)
    nn <- names(df)[map_lgl(df, ~length(.[[1]]) > 1)]
    df %>% unnest(cols = -{{nn}})
    })

  } else {
    df <- map_df(L, ~{
    Lnames <- names(.)
    .[map_lgl(., ~is.null(.))] <- NA_character_
    .[map_lgl(., ~length(.)==0)]  <- NA_character_
    names(.) <- Lnames
    df <- tibble(.) %>%
      pivot_longer(everything()) %>%
      mutate(name = Lnames) %>%
      pivot_wider(names_from = name, values_fn = NULL)
    nn <- names(df)[map_lgl(df, ~length(.[[1]]) > 1)]
    df %>% unnest(cols = -{{nn}})
    })
  }
  df
}
