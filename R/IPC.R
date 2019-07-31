# TODO remove all count variables

initialize.explanation.request <- function(con, comm_id, column_count) {
  requestParams = list("id" = comm_id,
                       "count" = 1,
                       "status" = "request",
                       "precision" = 0,
                       "instance" = column_count)

  instanceJSON = as.character(jsonlite::toJSON(requestParams, auto_unbox = T))
  writeLines(instanceJSON, con)
}

await.server.response <- function(con) {
  # check for response, if not available, listen again
  responseRaw = character(0)

  # TODO is busy waiting the way to go here?
  while (identical(responseRaw, character(0))) {
    responseRaw = readLines(con)
  }

  # get response
  response = jsonlite::fromJSON(responseRaw,
                                simplifyMatrix = F,
                                simplifyVector = T,
                                flatten = T,
                                simplifyDataFrame = F)
  response
}

respond.with.precision <- function(con, comm_id, matching_labels, precision) {
  responseList = list("id" = comm_id,
                      "count" = 1,
                      "status" = "eval_response",
                      "matchingLabels" = matching_labels,
                      "precision" = precision)
  instanceJSON = as.character(jsonlite::toJSON(responseList, auto_unbox = T))
  writeLines(instanceJSON, con)
}

respond.with.coverage <- function(con, comm_id, coverage) {
  responseList = list("id" = comm_id,
                      "count" = 1,
                      "status" = "coverage_response",
                      "coverage" = coverage)
  coverageJSON = jsonlite::toJSON(responseList, auto_unbox = T)
  writeLines(coverageJSON, con)
}
