function(){
library(httr)

# Log in as standard registry (bipmk30k) #gr1i4t22
{
  xLogin <- POST(url = "http://localhost:3000/api/v1/accounts/login",
                 body = list(username = "bipmk30k",
                             password = "test"),
                 encode = "json")
  xLogin <- parsed_content(xLogin)
}

# List available policies
{
  policies <- GET(url = "http://localhost:3000/api/v1/policies",
                  pageIndex = 0, pageSize = 10,
                  add_headers(Authorization = sprintf("Bearer %s",
                                                      xLogin$accessToken)))
  policies <- parsed_content(policies)

  policies[[1]]$id
}

# List all schemas available to standard registry (bipmk30k)
{
  schemas <- GET(url = "http://localhost:3000/api/v1/schemas",
                 pageIndex = 0, pageSize = 10,
                 add_headers(Authorization = sprintf("Bearer %s",
                                                     xLogin$accessToken)))
  schemas <- parsed_content(schemas)

  # Assign schema IRIs as list names
  names(schemas) <- sapply(X = schemas, FUN = function(x) x$iri)

}

# List all schemas related to a specific policy
{
  # Get policy topic ID
  topicId <- policies[[1]]$topicId

  schemas <- GET(url = sprintf("http://localhost:3000/api/v1/schemas/%s", topicId),
                 pageIndex = 0, pageSize = 10,
                 add_headers(Authorization = sprintf("Bearer %s",
                                                     xLogin$accessToken)))
  schemas <- parsed_content(schemas)

  # Assign schema IRIs as list names
  names(schemas) <- sapply(X = schemas, FUN = function(x) x$iri)

}

# Get a specific policy
{
  policy <- GET(url = sprintf("http://localhost:3000/api/v1/policies/%s",
                              policies[[1]]$id),
                add_headers(Authorization = sprintf("Bearer %s",
                                                    xLogin$accessToken)))
  policy <- parsed_content(policy)

  # Iterate through the policy (a list) and assign the block's tag to the list
  # item as its name.
  assignNames <- function(x) {

    if ("children" %in% names(x)) {
      if (length(x$children) > 0) {
        names(x$children) <- sapply(X = x$children, FUN = function(y) y$tag)
        x$children <- lapply(X = x$children, FUN = assignNames)
      }
    }

    return(x)
  }

  policy$config <- assignNames(policy$config)

}

# Get info about a specific block in the policy (not necessary)
{
  x <- GET(url = sprintf("http://localhost:3000/api/v1/policies/%s/blocks/%s",
                         policy$id,
                         policy$config$children$icb_projDev$children$isb_projDev$children$rvcdb_schemaA$id),
           add_headers(Authorization = sprintf("Bearer %s",
                                               xLogin$accessToken)))
  x <- parsed_content(x)
  x

  # Hhmmm, why is this NULL?
}

# Send data to a specific block in the policy (as user rt7y35un)
{
  # Log in as a non-SR user associated with the policy.
  xLogin <- POST(url = "http://localhost:3000/api/v1/accounts/login",
                 body = list(username = "rt7y35un",
                             password = "test"),
                 encode = "json")
  xLogin <- parsed_content(xLogin)

  # Choose role.
  # TODO.

  # Submit the actual data to 'rvcdb_schemaA' block.
  schemaID <- policy$config$children$icb_projDev$children$isb_projDev$children$rvcdb_schemaA$schema
  schemaIPFSurl <- schemas[[schemaID]]$contextURL

  x <- POST(url = sprintf("http://localhost:3000/api/v1/policies/%s/blocks/%s",
                          policy$id,
                          policy$config$children$icb_projDev$children$isb_projDev$children$rvcdb_schemaA$id),
            add_headers(Authorization = sprintf("Bearer %s",
                                                xLogin$accessToken)),
            body = list(document = list(field0 = "Jane",
                                        field1 = "Doe",
                                        type = gsub(pattern = "^#{1}", replacement = "", x = schemaID),
                                        '@context' = schemaIPFSurl)),
            encode = "json")
  x
  x <- parsed_content(x)
  x


  # Submit the actual data to 'rvcdb_schemaB' block.
  schemaID <- policy$config$children$icb_projDev$children$isb_projDev$children$rvcdb_schemaB$schema
  schemaIPFSurl <- schemas[[schemaID]]$contextURL

  x <- POST(url = sprintf("http://localhost:3000/api/v1/policies/%s/blocks/%s",
                          policy$id,
                          policy$config$children$icb_projDev$children$isb_projDev$children$rvcdb_schemaB$id),
            add_headers(Authorization = sprintf("Bearer %s",
                                                xLogin$accessToken)),
            body = list(document = list(field0 = "Canada",
                                        type = gsub(pattern = "^#{1}", replacement = "", x = schemaID),
                                        '@context' = schemaIPFSurl)),
            encode = "json")
  x
  x <- parsed_content(x)
  x

}

# Example of what body of POST would have looked like if Schema A had contained a third field (field3) of
# which the type was our ICP's Location schema, and we've chosen to specify the location data as a POINT.
{
  list(field0 = "Jane",
       field1 = "Doe",

       field3 = list(
         format_location = "POINT",

         point = list(longitude = 1234.234234,
                      latitute = 12343.234234,
                      elevation = 50,
                      crs = "WGS 84",
                      type = "f5242377-bbdc-4659-929f-9b356f982aae&1.0.0",
                      '@context' = "ipfs://bafkreiasxuwjzmxzlmjesxeayoojw44nv4cigjw33zdjr43rl7uehcmi7e"),

         type = "2f65972e-91db-4021-82b5-0285c831b57d&1.0.0",
         '@context' = "ipfs://bafkreicypaamzocscenqanjtn5vk6fabidvpwpye2j6osy7dfxnx5yryji"),

       type = gsub(pattern = "^#{1}", replacement = "", x = schemaID),
       '@context' = schemaIPFSurl)
}

# TrustChain APIs
GET /trustchains #Returns a list of all VP documents
GET /trustchains/{hash} # Returns a trustchain for a VP document

# Send Data using the External Data APIs
POST /external # Sends data from an external source.

# Standard Registry Operations
## Settings APIs
GET /settings #Returns current settings
POST /settings #Set settings.
##  Logs APIs
POST /logs # Returns logs.
GET /logs/attributes #Returns logs attributes
## Task Statuses APIs
GET /tasks/{taskId} #Returns task statuses.

# Users
## Account APIs
GET /accounts #Returns a list of users, excluding Standard Registry and Auditors
GET /accounts/balance #Returns user
GET /accounts/session #Returns current session of the user
POST /accounts/login #Logs user into the system
POST /accounts/register #Registers a new user account
GET /accounts/standard-registries #Returns an array of Standard Registries for user to select one during registration process
## Profile APIs
GET /profiles/{username}/balance #Returns user's Hedera account balance
GET /profiles/{username} #Returns user account info
PUT /profiles/{username} #Sets Hedera credentials for the user
PUT /profiles/push/{username} # Sets Hedera credentials for the user

# Artifacts APIs
GET /artifact
POST /artifact/{policyId}
DELETE /artifact/{artifactId}

GET /contracts
POST /contracts

POST /contracts/import
POST  /contracts/{contractId}/user
POST /contracts/{contractId}/status
POST /contracts/{contractId}/pair
GET /contracts/pair
GET /contracts/{contractId}/retire/request
GET /contracts/retire/request
DELETE /contracts/retire/request
POST /contracts/retire

POST /schemas
GET /schemas

PUT /schemas/{schemaId}/publish
PUT /schemas/{schemaId} # update
DELETE /schema/{schemaID}

POST /schemas/import/file #Imports new schema from a zip file
POST /schemas/import/message # Imports new schema from IPFS
POST /schemas/import/message/preview #Schema preview from IPFS
POST /schemas/import/file/preview # Schema preview from a zip file
POST /schemas/{schemaId}/export/message #Hedera message IDs of published schemas
POST /schemas/{schemaId}/export/file #Return zip file with schemas
POST /schemas/{topicId} # Creates a schema related to the topic (policy)
GET  /schemas/{topicId} #Returns schemas related to the topic (policy)
POST /schemas/{topicId}/import/file #Imports schemas from a file for the selected topic (policy)
POST /schemas/{topicId}/import/message #Imports schemas from a message for the selected topic (policy)
GET /schema/{schemaId} #Returns schema by schema ID

GET /schemas/type/{type} # Returns Schema by Type
POST /schemas/system/{username} #Creates new System Schema
GET /schemas/system/{username} #Returns all System Schemas by Username
PUT /schemas/system/{schemaId} #Updates the Schema
DELETE /schemas/system/{schemaId} #Deletes the Schema
PUT /schemas/{schemaId}/active #Publishes the Schema
GET /schemas/system/entity/{schemaEntity} #Returns Schema by Schema Type

# Schema APIs for Asynchronous Execution
POST /schemas/push/{topicId} #Create new schema.
PUT /schemas/push/{schemaId}/publish # Publishes the schema with the provided (internal) schema ID onto IPFS, sends a message featuring IPFS CID into the corresponding Hedera topic.
POST /schemas/push/import/message/preview #Schema preview from IPFS
POST /schemas/push/{topicId}/import/message # Imports new schema from IPFS.
POST /schemas/push/{topicId}/import/file #Imports new schema from a zip file.

# Creating a Policy using APIs
POST /api/v1/accounts/register #New Standard Registry registration
POST /api/v1/accounts/login #Login
GET /api/v1/demo/randomKey #Hedera account creation
PUT /api/v1/profile #Address book creation

POST /api/v1/tokens #Token creation
POST /policies #Creates a new policy
GET /policies #Return a list of all policies
POST /policies/import/message #Imports new policy from IPFS.
POST /policies/import/message/preview #Policy preview from IPFS
GET /policies/{policyId} #Retrieves policy configuration
PUT /policies/{policyId} #Updates policy configuration
PUT /policies/{policyId}/publish # Publishes the policy onto IPFS
POST /policies/validate #Validates policy
GET /policies/{policyId}/blocks #Retrieves data for the policy root block
GET /policies/{policyId}/blocks/{uuid} #Requests block data
POST /policies/{policyId}/blocks/{uuid} #Sends data to the specified block
GET /policies/{policyId}/tag/{tag} #Requests block ID from a policy by tag
GET /policies/{policyId}/export/message #Return Hedera message ID for the specified published policy
GET /policies/{policyId}/export/file #Return policy and its artifacts in a zip file format for the specified policy
POST /policies/import/file #Imports new policy from a zip file
GET /policies/{policyId}/tag/{tag}/blocks #Requests block data
POST /policies/{policyId}/tag/{tag}/blocks #Sends data to the specified block.
GET /policies/{policyId}/groups #Returns a list of groups the user is a member of.
POST /policies/{policyId}/groups # Makes the selected group active.

# Creating link between policies
POST /policies/{policyId}/multiple # Requests Multi policy config
GET /policies/{policyId}/multiple # Requests Multi policy config
}


