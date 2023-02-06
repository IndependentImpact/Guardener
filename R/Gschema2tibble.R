

#' Gschema2tibble
#' @description take the list comming out of GgetSchemas and make a tibble with 1 schema detail per row
#' @param schemas list
#' @return tibble
#' @export
#'
#' @examples
Gschema2tibble <- function(schemas){
  dfSchemas <- map_df(schemas, Glist2tibble) %>% unnest(everything())
  dfSchemas$document <- map(dfSchemas$document, Gdocument2tibble)
  dfSchemas$context <- map(dfSchemas$context, ~flatten(Gdocument2tibble(.)))
  dfSchemas$context <- map(dfSchemas$context, flatten)
  dfSchemas$fields <- map2(dfSchemas$context, dfSchemas$uuid, ~..1[[..2]][["@context"]])
  dfSchemas$version  <- map(dfSchemas$context,  ~..1[["@version"]])
  dfSchemas$fieldnames <- map(dfSchemas$fields, ~names(.))
  dfSchemas$fieldtypes <- map(dfSchemas$fields, ~names(.))
  dfSchemas %>% unnest(c("fields", "fieldnames")) %>% unnest(fields) %>% unnest(fields)
}

# Gschema2tibble(GgetSchemas(AT)) %>% select(`_id`, name, description, system, active, fields, fieldnames) %>% group_by(name) %>% nest()
#  dfpars <- map_df(dfSchemas$document, ~pluck(., "properties", 1, 4))


