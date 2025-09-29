bookmarkRootDir = "shiny_bookmarks" # it seems impossible to change the root-dir :/
bookmarkDir <- paste0(bookmarkRootDir, "/latest")
# `input.rds` is the expected file name by shiny!
bookmarkFileName <- "input.rds"
bookmarkRdsTargetPath <- fs::path(bookmarkDir, bookmarkFileName)
# `input.json` is a custom file-name and a custom file-content to access the shiny values in plain text
bookmarkJsonName <- "input.json"
bookmarkJsonTargetPath <- fs::path(bookmarkDir, bookmarkJsonName)

#' Save Shiny Bookmark as Latest
#'
#' Moves a Shiny bookmark from its temporary location to a persistent "latest" location.
#' This function extracts the state ID from a bookmark URL, moves the bookmark files
#' to a standardized location, and cleans up the temporary directory.
#'
#' @param url Character string. The bookmark URL containing the state ID parameter.
#'   Expected to contain a `_state_id_` query parameter.
#'
#' @return No return value, called for side effects (file operations and logging).
#'
#' @details
#' The function performs the following operations:
#' \enumerate{
#'   \item Extracts the `_state_id_` parameter from the provided URL
#'   \item Creates the target "latest" bookmark directory if it doesn't exist
#'   \item Moves the bookmark file from the temporary state directory to the latest location
#'   \item Removes the temporary state directory to clean up
#'   \item Logs the operation for debugging purposes
#' }
#'
#' The bookmark file is expected to be named "input.rds" as required by Shiny's
#' bookmark system.
#'
#' @examples
#' \dontrun{
#' # Save bookmark from a Shiny bookmark URL
#' bookmark_url <- "http://localhost:3838/?_state_id_=abc123"
#' saveBookmarkAsLatest(bookmark_url)
#' }
#'
#' @seealso \code{\link{restoreShinyBookmark}} for restoring bookmarks
saveBookmarkAsLatest <- function(url) {
  stateId <- shiny::parseQueryString(sub("^.*\\?", "", url))$`_state_id_`
  if(!fs::dir_exists(bookmarkDir)){
    fs::dir_create(bookmarkDir)
  }
  fs::file_move(
    path = fs::path("shiny_bookmarks", stateId, bookmarkFileName),
    new_path = bookmarkRdsTargetPath
  )
  fs::dir_delete(fs::path("shiny_bookmarks", stateId))
  logger.debug(paste("[bookmark] Moved shiny bookmark", stateId, "to", bookmarkDir))
}

#' Restore Shiny Bookmark
#'
#' Automatically restores the latest Shiny bookmark if one exists and hasn't been
#' loaded yet. This function checks for an existing bookmark and reloads the session
#' with the appropriate state ID to restore the application state.
#'
#' @param session A Shiny session object, typically provided by the Shiny server function.
#'
#' @return No return value, called for side effects (session reload and logging).
#'
#' @details
#' The function performs the following checks and operations:
#' \enumerate{
#'   \item Checks if a bookmark file exists in the "latest" location
#'   \item Verifies that no state ID is currently present in the session URL
#'   \item If both conditions are met, updates the query string to load the "latest" bookmark
#'   \item Reloads the session to apply the bookmarked state
#'   \item Logs the operation for debugging purposes
#' }
#'
#' This function should typically be called during Shiny application initialization
#' to automatically restore the user's last saved state.
#'
#' @examples
#' \dontrun{
#' # In a Shiny server function
#' server <- function(input, output, session) {
#'   # Restore bookmark on app start
#'   restoreShinyBookmark(session)
#'
#'   # ... rest of server logic
#' }
#' }
#'
#' @seealso \code{\link{saveBookmarkAsLatest}} for saving bookmarks
restoreShinyBookmark <- function(session) {
  if(fs::file_exists(bookmarkRdsTargetPath) && is.null(shiny::parseQueryString(session$clientData$url_search)$`_state_id_`)) {
    shiny::updateQueryString(queryString = "?_state_id_=latest")
    logger.debug("[bookmark] Reloading session b/c of detected (not yet loaded) shiny bookmark")
    session$reload()
  }
}

#' Save Shiny Input as JSON
#'
#' Saves Shiny input values as a JSON file for external access and debugging purposes.
#' This creates a human-readable plain text version of the Shiny input state that
#' can be accessed and analyzed outside of the R environment.
#'
#' @param jsonString Character string. A JSON-formatted string containing the Shiny
#'   input values to be saved.
#'
#' @return No return value, called for side effects (file writing and logging).
#'
#' @details
#' The function attempts to write the provided JSON string to a file named "input.json"
#' in the bookmark directory. This provides a plain text alternative to the binary
#' RDS format used by Shiny's built-in bookmark system.
#'
#' Error handling is implemented to catch and log any file writing failures without
#' stopping the application execution.
#'
#' @examples
#' \dontrun{
#' # Save current input state as JSON
#' input_json <- jsonlite::toJSON(reactiveValuesToList(input), auto_unbox = TRUE)
#' saveInputAsJson(input_json)
#'
#' # Save custom state as JSON
#' custom_state <- '{"slider_value": 50, "text_input": "hello"}'
#' saveInputAsJson(custom_state)
#' }
#'
#' @seealso \code{\link{jsonlite::toJSON}} for converting R objects to JSON,
#' \code{\link{reactiveValuesToList}} for extracting Shiny input values
saveInputAsJson <- function(jsonString) {
  tryCatch(
    {
      writeLines(jsonString, bookmarkJsonTargetPath)
      logger.debug("[bookmark] Persisted shiny input as JSON")
    },
    error = function(e) {
      logger.error(paste("[bookmark] Could not write shiny input JSON file:", e))
    })
}