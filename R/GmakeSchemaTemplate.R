
#' GmakeSchemaTemplate
#' @description Make a nested tibble by schema. The data fields and their types are in the nested column
#' @param dfSchemas tibble. Output of Gschema2tibble()
#' @return tibble
#' @export

GmakeSchemaTemplate <- function(dfSchemas){
  z <- dfSchemas %>%
    select(`_id`, uuid, name, fields, fieldnames) %>%
    group_by(name, uuid) %>%
    filter(!fieldnames  %in% c("ref", "policyId")) %>%
    mutate(fields = gsub("#", "", fields),
           schemafield = fields %in% .$uuid,
           inschema = case_when(schemafield ~ fields, TRUE ~ uuid)
           ) %>%
    group_by(name, uuid, inschema, schemafield) %>%
    tidyr::nest()

  if (!any(z$schemafield)) return(z)

  ids <- map_chr(z$data[which(z$schemafield)], ~.[1,2][[1]])
  z$data[z$schemafield] <- map(ids, ~getSchemaInside(z, .))

  IdAndReplaceSchema(z)

  # z <- z %>%
  #   ungroup() %>%
  #   select(-schemafield, -inschema) %>%
  #   tidyr::unnest(cols = c(data)) %>%
  #   select(-inschema)
}

getSchemaInside <- function(z, id){
  z %>% tidyr::unnest(data) %>% ungroup() %>%
    filter(uuid == id) %>%
    select(uuid, fields, fieldnames) %>%
    set_names("inschema", "fields", "fieldnames")
}

IdAndReplaceSchema <- function(z){
  if (!"schemafield" %in% colnames(z)) {
    message("I am making a schemafield")

    z <- z %>%
      ungroup() %>%
      select(-inschema, -schemafield) %>%
      tidyr::unnest(cols = c(data)) %>%
      mutate(fields = gsub("#", "", fields),
                schemafield = fields %in% .$uuid,
                inschema = case_when(schemafield ~ fields, TRUE ~ uuid)
                ) %>%
      group_by(name, uuid, inschema, schemafield) %>%
      tidyr::nest()

  }

  if (!any(z$schemafield)) return(z)
  ids <- map_chr(z$data[which(z$schemafield)], ~.[1,2][[1]])
  z$data[z$schemafield] <- map(ids, ~getSchemaInside(z, .))
  z
}



# ids <- map_chr(z$data[which(z$schemafield)], ~.[1,2][[1]])
# map(ids, ~getSchemaInside(z, .))
# z$data[z$schemafield] <- map(ids, ~getSchemaInside(z, .))

