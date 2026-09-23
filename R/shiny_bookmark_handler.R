bookmarkRootDir = "shiny_bookmarks" # it seems impossible to change the root-dir :/
bookmarkDir <- paste0(bookmarkRootDir, "/latest")
# `input.rds` is the expected file name by shiny!
bookmarkFileName <- "input.rds"
bookmarkRdsTargetPath <- fs::path(bookmarkDir, bookmarkFileName)
# `input.json` is a custom file-name and a custom file-content to access the shiny values in plain text
bookmarkJsonName <- "input.json"
bookmarkJsonTargetPath <- fs::path(bookmarkDir, bookmarkJsonName)
# query parameter marking a session reload that should start with the default settings of this App
restoreDefaultsQueryParam <- "_ma_defaults_"
# one-time tokens (as names) of requested reloads with the default settings; a URL with an unknown
# token (e.g. an old or copied URL) must not replace the stored settings
restoreDefaultsTokens <- new.env(parent = emptyenv())

ensureBookmarkDirExists <- function() {
  if(!fs::dir_exists(bookmarkDir)){
    fs::dir_create(bookmarkDir)
    logger.debug(paste("[bookmark] Created shiny bookmark directory", bookmarkDir))
  }
}

#' Save Shiny Bookmark as Latest
#'
#' Moves a Shiny bookmark from its temporary location to a persistent "latest" location.
#' This function extracts the state ID from a bookmark URL, moves the bookmark files
#' to a standardized location, and cleans up the temporary directory.
#'
#' @param url Character string. The bookmark URL containing the state ID parameter.
#'   Expected to contain a `_state_id_` query parameter.
#'
#' @return Invisibly \code{TRUE} if the bookmark was saved, otherwise invisibly
#'   \code{FALSE}. Called mainly for side effects (file operations and logging).
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
#' @export
saveBookmarkAsLatest <- function(url) {
  tryCatch(
    {
      stateId <- shiny::parseQueryString(sub("^.*\\?", "", url))$`_state_id_`
      ensureBookmarkDirExists()

      fs::file_move(
        path = fs::path("shiny_bookmarks", stateId, bookmarkFileName),
        new_path = bookmarkRdsTargetPath
      )
      fs::dir_delete(fs::path("shiny_bookmarks", stateId))
      logger.debug(paste("[bookmark] Moved shiny bookmark", stateId, "to", bookmarkDir))
      invisible(TRUE)
    },
    error = function(e) {
      logger.error(paste("[bookmark] Could not save the shiny bookmark as latest:", e))
      invisible(FALSE)
    }
  )
}

#' Restore Shiny Bookmark
#'
#' Automatically restores the latest Shiny bookmark if one exists and hasn't been
#' loaded yet. This function checks for an existing bookmark and reloads the session
#' with the appropriate state ID to restore the application state.
#'
#' @param session A Shiny session object, typically provided by the Shiny server function.
#'
#' @return Invisibly \code{TRUE} if default settings were requested via
#'   \code{\link{restoreDefaultSettings}} (no bookmark is restored then), otherwise
#'   invisibly \code{FALSE}. Called mainly for side effects (session reload and logging).
#'
#' @details
#' The function performs the following checks and operations:
#' \enumerate{
#'   \item Skips the restore if default settings were requested via
#'     \code{\link{restoreDefaultSettings}} (and removes that request from the URL).
#'     A request with an unknown token (e.g. from an old or copied URL) is ignored.
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
#' @export
restoreShinyBookmark <- function(session) {
  tryCatch(
    {
      queryString <- shiny::parseQueryString(session$clientData$url_search)
      defaultsToken <- queryString[[restoreDefaultsQueryParam]]
      if (!is.null(defaultsToken)) {
        # drop the marker, so that a later reload restores the stored settings again
        shiny::updateQueryString(queryString = "?", mode = "replace", session = session)
        if (exists(defaultsToken, envir = restoreDefaultsTokens, inherits = FALSE)) {
          # the user asked for the default settings: do not restore any bookmark
          rm(list = defaultsToken, envir = restoreDefaultsTokens)
          logger.debug("[bookmark] Skipped restoring the shiny bookmark b/c default settings were requested")
          return(invisible(TRUE))
        }
        logger.warn("[bookmark] Ignored a request for the default settings with an unknown token")
      }
      if(fs::file_exists(bookmarkRdsTargetPath) && is.null(queryString$`_state_id_`)) {
        shiny::updateQueryString(queryString = "?_state_id_=latest", session = session)
        logger.debug("[bookmark] Reloading session b/c of detected (not yet loaded) shiny bookmark")
        session$reload()
      }
    },
    error = function(e) {
      logger.error(paste("[bookmark] Could not restore the shiny bookmark:", e))
    }
  )
  invisible(FALSE)
}

#' Restore Default Settings
#'
#' Deletes the stored settings and reloads the Shiny session without restoring any
#' bookmark, so that all inputs show the default values defined by this App in
#' \code{shinyModuleUserInterface}.
#'
#' @param session A Shiny session object, typically provided by the Shiny server function.
#'
#' @return No return value, called for side effects (file deletion, session reload and logging).
#'
#' @details
#' The function deletes the stored settings (\code{input.rds} and \code{input.json}
#' of the "latest" bookmark) and reloads the session with a marker in the query string.
#' The marker carries a one-time random token and tells \code{\link{restoreShinyBookmark}}
#' to skip the automatic restore.
#' \code{\link{createMoveAppsShinyServer}} then stores the default settings, which
#' also replaces the copy of the stored settings on MoveApps.
#'
#' @examples
#' \dontrun{
#' # In a Shiny server function
#' observeEvent(input$ma_restore_defaults, {
#'   restoreDefaultSettings(session)
#' })
#' }
#'
#' @seealso \code{\link{restoreShinyBookmark}} for restoring bookmarks
#' @export
restoreDefaultSettings <- function(session) {
  tryCatch(
    {
      storedSettings <- c(bookmarkRdsTargetPath, bookmarkJsonTargetPath)
      fs::file_delete(path = storedSettings[fs::file_exists(path = storedSettings)])
      logger.debug("[bookmark] Deleted the stored shiny bookmark")
      # `tempfile()` creates a random name without changing the random number generator state of this App
      defaultsToken <- basename(tempfile(pattern = ""))
      assign(defaultsToken, TRUE, envir = restoreDefaultsTokens)
      shiny::updateQueryString(queryString = paste0("?", restoreDefaultsQueryParam, "=", defaultsToken), mode = "replace", session = session)
      logger.debug("[bookmark] Reloading session to restore the default settings of this App")
      session$reload()
    },
    error = function(e) {
      logger.error(paste("[bookmark] Could not restore the default settings:", e))
    }
  )
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
#' @export
saveInputAsJson <- function(jsonString) {
  tryCatch(
    {
      ensureBookmarkDirExists()
      writeLines(jsonString, bookmarkJsonTargetPath)
      logger.debug("[bookmark] Persisted shiny input as JSON")
    },
    error = function(e) {
      logger.error(paste("[bookmark] Could not write shiny input JSON file:", e))
    }
  )
}