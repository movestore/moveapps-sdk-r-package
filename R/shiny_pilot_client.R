
#' Get Pilot Endpoint URL
#'
#' Retrieves the pilot endpoint URL from the PILOT_ENDPOINT environment variable.
#' If not set, defaults to localhost on port 8100.
#'
#' @return Character string containing the pilot endpoint URL
#' @examples
#' \dontrun{
#' endpoint <- pilotEndpoint()
#' # Returns "http://localhost:8100" by default
#' }
pilotEndpoint <- function() {
  Sys.getenv(x = "PILOT_ENDPOINT", "http://localhost:8100")
}

#' Check HTTP Client Logging Status
#'
#' Determines whether HTTP client logging is enabled based on the
#' PILOT_CLIENT_LOG_LEVEL environment variable.
#'
#' @return Logical value indicating whether HTTP client logging is enabled
#' @examples
#' \dontrun{
#' if (httpClientLogging()) {
#'   # Enable verbose HTTP logging
#' }
#' }
httpClientLogging <- function() {
  httpClientLogging <- Sys.getenv(x = "PILOT_CLIENT_LOG_LEVEL", "NULL") # real NULL not possible (?!)
  httpClientLogging != "NULL"
}

#' Notify Pilot of Execution Completion
#'
#' Sends a notification to the pilot service indicating that an execution has
#' completed successfully. This function is used internally by the MoveApps framework.
#'
#' @param executionType Character string specifying the type of execution that completed
#' @return No return value. Called for its side effect of sending HTTP notification.
#' @details
#' If the HTTP_CLIENT_FAKE environment variable is set to "yes", the function
#' returns early without sending any notification (useful for testing).
#' @export
#' @examples
#' \dontrun{
#' notifyDone("shiny_app")
#' }
notifyDone <- function(executionType) {
  if (Sys.getenv(x = "HTTP_CLIENT_FAKE", "no") == "yes") {
    logger.debug("interrupted notify done as app is running w/ HTTP_CLIENT_FAKE=yes")
    return()
  }
  response <- httr::POST(
    paste(pilotEndpoint(), "/pilot/api/v1/copilot/done", sep = ""),
    body = jsonlite::toJSON(list("success" = TRUE, "executionType" = executionType), auto_unbox = TRUE),
    encode = "json",
    httr::content_type_json(),
    if (httpClientLogging()) httr::verbose(info = TRUE, data_out = TRUE, data_in = TRUE)
  )
  logger.debug("notify done with success")
}

#' Push Shiny Bookmark to Pilot
#'
#' Notifies the pilot service to push a Shiny bookmark file. This function is used
#' to save and restore Shiny application state.
#'
#' @param fileName Character string specifying the name of the bookmark file
#' @return No return value. Called for its side effect of uploading the bookmark.
#' @details
#' If the HTTP_CLIENT_FAKE environment variable is set to "yes", the function
#' returns early without sending any notification (useful for testing).
#' The function logs whether the upload was successful or failed.
#' @export
#' @examples
#' \dontrun{
#' notifyPushBookmark("bookmark_2023-10-01.rds")
#' }
notifyPushBookmark <- function(fileName) {
  if (Sys.getenv(x = "HTTP_CLIENT_FAKE", "no") == "yes") {
    logger.debug("interrupted notify push boorkmark as app is running w/ HTTP_CLIENT_FAKE=yes")
    return()
  }
  logger.debug(paste("Notify pilot to push bookmark", fileName))

  response <- httr::POST(
    paste(pilotEndpoint(), "/pilot/api/v1/copilot/shiny-bookmark", sep = ""),
    body = fileName,
    httr::content_type("text/plain"),
    httr::accept_json(),
    if (httpClientLogging()) httr::verbose(info = TRUE, data_out = TRUE, data_in = TRUE)
  )

  parsedResponse <- httr::content(response, "parsed")

  if (parsedResponse["success"] == TRUE) {
      logger.debug("Uploaded shiny-bookmark")
    } else {
      logger.info("Couldn't upload shiny-bookmark")
    }
}

#' Store Configuration in Pilot
#'
#' Stores application configuration in the pilot service and updates the local
#' CONFIGURATION environment variable with the response.
#'
#' @param configuration The configuration object to be stored (will be converted to JSON)
#' @return No return value. Called for its side effect of storing configuration.
#' @details
#' If the HTTP_CLIENT_FAKE environment variable is set to "yes", the function
#' returns early without sending any request (useful for testing).
#' On successful storage, the function updates the CONFIGURATION environment
#' variable with the configuration returned from the server.
#' @export
#' @examples
#' \dontrun{
#' config <- list(setting1 = "value1", setting2 = 42)
#' storeConfiguration(config)
#' }
storeConfiguration <- function(configuration) {
  if (Sys.getenv(x = "HTTP_CLIENT_FAKE", "no") == "yes") {
    logger.debug("interrupted store configuration as app is running w/ HTTP_CLIENT_FAKE=yes")
    return()
  }
  logger.debug("Storing configuration in pilot: %s", configuration)

  response <- httr::POST(
    paste(pilotEndpoint(), "/pilot/api/v1/copilot/configuration", sep = ""),
    body = jsonlite::toJSON(list("configuration" = configuration), auto_unbox = TRUE),
    encode = "json",
    httr::content_type_json(),
    if (httpClientLogging()) httr::verbose(info = TRUE, data_out = TRUE, data_in = TRUE)
  )

  parsedResponse <- httr::content(response, "parsed")

  if (parsedResponse["success"] == TRUE) {
    newConfiguration = jsonlite::toJSON(parsedResponse[["configuration"]], auto_unbox = TRUE)
    Sys.setenv(CONFIGURATION = newConfiguration)
    logger.debug("Set new configuration environment: %s", newConfiguration)
  } else {
    logger.info("Couldn't store Configuration")
  }
}