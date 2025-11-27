# Guardener

Guardener is an R package that wraps the Hedera Guardian API with helper functions for authentication, policy discovery, schema inspection, and data preparation. It is intended to make it easier to explore Guardian policies from R and to build payloads that conform to policy schemas.

## Installation

You can install the package from a local checkout using [`devtools`](https://devtools.r-lib.org/):

```r
# install.packages("devtools")
devtools::install_local(".")
```

## Authentication

Most functions expect a Guardian refresh token, which you can obtain with `Glogin()`. By default the function reads credentials from environment variables, so you can keep secrets out of scripts:

```r
RT <- Glogin(un = Sys.getenv("GUARDENER_USERNAME"),
             pw = Sys.getenv("GUARDENER_PASSWORD"),
             baseurl = "http://localhost:3000/")
refresh_token <- RT$refreshToken
```

`Glogin()` returns the username, DID, role, and tokens, and will raise an error if authentication fails. The optional `baseurl` lets you point to a remote Guardian instance instead of the local default. Use `GgetAccessToken()` whenever you need a short-lived access token for subsequent API calls.

## Core workflows

### List available policies

Use `GgetPolicies()` with your refresh token to list policies accessible to the current role. Set `returndf = TRUE` to receive a tibble instead of the raw API response. Paging parameters allow you to control the number of results returned.

```r
df_policies <- GgetPolicies(refresh_token,
                            baseurl = "http://localhost:3000/",
                            returndf = TRUE)
```

### Inspect a policy and its blocks

`GgetPolicyConfig()` (used internally by `GgetPolicyBlocks()`) unwraps the nested policy configuration. For a quick view of actionable blocks and their schemas, call `GgetPolicyBlocks()` and filter the results by `blockType`.

```r
blocks <- GgetPolicyBlocks(refreshToken = refresh_token,
                           policyId = df_policies$id,
                           baseurl = "http://localhost:3000/")
request_blocks <- dplyr::filter(blocks, blockType == "requestVcDocumentBlock")
```

### Retrieve schemas

Use `GgetSchemas()` to fetch schemas either for a policy topic or across the registry. When you only need a specific schema, provide `schemaId` to bypass the broader query.

```r
df_schemas <- GgetSchemas(refreshToken = refresh_token,
                          baseurl = "http://localhost:3000/",
                          returndf = TRUE)
```

### Build schema-driven templates

`GmakeSchemaTemplate()` transforms schema metadata into nested tibbles that mirror the required document structure. You can filter the tibble of schemas produced by `GgetSchemas()` to a target schema name and then call `GmakeSchemaTemplate()` to derive a template for data entry or validation.

```r
mr_template <- df_schemas %>%
  dplyr::filter(name == "Monitoring Report (MR)") %>%
  GmakeSchemaTemplate()
```

## Example session

The included vignette walks through a full session: loading credentials from environment variables, logging in, listing policies, inspecting blocks, and preparing schema templates. See `vignettes/Overview.Rmd` for executable examples you can adapt to your own Guardian deployment.
