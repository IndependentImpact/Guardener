

#' nestedSchemaTemplate
#' @description Make a template for a schema with sub-schema field nested in list-columns
#' @param dfSchemas tibble.  Output of Gschema2tibble()
#' @param id character. id field in dfSchema of schema for which you want to make the template
#' @return tibble
#' @export

nestedSchemaTemplate <- function(dfSchemas = NULL, id = NULL, uuid = "uuid"){
  if (is.null(dfSchemas)) stop("You have to provide dfSchemas")
  if (is.null(id)) stop("You have to provide id")
  if (!id %in% dfSchemas$id) stop("No value like ", id, " in dfSchemas$id")

  df <- dfSchemas %>% filter(id == {{id}} & !fieldnames %in% c("ref", "policyId"))

  fields <- df$fieldnames
  types <- df$fields %>% unname()
  schemanames <- types[!types %in% "https://www.schema.org/text"]
  schemafields <- fields[!types %in% "https://www.schema.org/text"]

  df1 <- tibble(k = NA_character_, fields = c(uuid, setdiff(fields, schemafields)))  %>%
    pivot_wider( names_from = fields, values_from = k) %>%
    unnest(everything())
  df1$uuid <- uuid
  df1

  if (length(schemanames) > 0 ) {
    df1
  } else {return(df1)}

  }
