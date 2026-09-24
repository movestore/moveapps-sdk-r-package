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
#' @return Invisibly, how this session starts: \code{"defaults"} if default settings were
#'   requested via \code{\link{restoreDefaultSettings}} (no bookmark is restored then),
#'   \code{"reloading"} if the session is reloaded to restore the stored settings, otherwise
#'   \code{"none"}. Called mainly for side effects (session reload and logging).
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
      queryString <- shiny::getQueryString(session)
      defaultsToken <- queryString[[restoreDefaultsQueryParam]]
      if (!is.null(defaultsToken)) {
        # drop the marker, so that a later reload restores the stored settings again
        shiny::updateQueryString(queryString = "?", mode = "replace", session = session)
        if (exists(defaultsToken, envir = restoreDefaultsTokens, inherits = FALSE)) {
          # the user asked for the default settings: do not restore any bookmark
          rm(list = defaultsToken, envir = restoreDefaultsTokens)
          logger.debug("[bookmark] Skipped restoring the shiny bookmark b/c default settings were requested")
          return(invisible("defaults"))
        }
        logger.warn("[bookmark] Ignored a request for the default settings with an unknown token")
      }
      if(fs::file_exists(bookmarkRdsTargetPath) && is.null(queryString$`_state_id_`)) {
        shiny::updateQueryString(queryString = "?_state_id_=latest", session = session)
        logger.debug("[bookmark] Reloading session b/c of detected (not yet loaded) shiny bookmark")
        session$reload()
        return(invisible("reloading"))
      }
    },
    error = function(e) {
      logger.error(paste("[bookmark] Could not restore the shiny bookmark:", e))
    }
  )
  invisible("none")
}

#' Restore Default Settings
#'
#' Reloads the Shiny session without restoring any bookmark, so that all inputs show the
#' default values defined by this App in \code{shinyModuleUserInterface}. These default
#' settings then replace the stored settings.
#'
#' @param session A Shiny session object, typically provided by the Shiny server function.
#'
#' @return No return value, called for side effects (session reload and logging).
#'
#' @details
#' The function reloads the session with a marker in the query string. The marker
#' carries a one-time random token and tells \code{\link{restoreShinyBookmark}} to skip
#' the automatic restore. \code{\link{createMoveAppsShinyServer}} then stores the default
#' settings, which replaces the stored settings (\code{input.rds} and \code{input.json}
#' of the "latest" bookmark), also on MoveApps. The stored settings are not deleted
#' beforehand: if storing the defaults fails, they stay in sync with the copy on MoveApps.
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

#' Find Stored Settings Which Could Not Be Applied
#'
#' Compares the stored value of each setting with the value shown after restoring it.
#' A difference means that the stored value could not be applied, e.g. because the
#' choices created via \code{renderUI} from the input data do not contain it anymore.
#' An upload (\code{fileInput}) is compared without its \code{datapath}: shiny copies a
#' restored upload to a new temporary path (e.g. \code{0.gpkg} to \code{/tmp/Rtmp.../0.gpkg}).
#' It only counts as applied if that file exists: shiny's copy fails silently if the bookmark
#' did not keep the file, and still hands the App the path. A setting stored with nothing
#' selected (\code{NULL}) fits any data: shiny's \code{radioButtons} shows its first choice
#' for it, which would otherwise count as not applied on every run.
#'
#' @param storedInputs Named list of the stored input values (content of \code{input.rds}).
#' @param currentInputs Named list of the current input values.
#' @param settingIds Character vector of the ids of the settings shown in the UI.
#'
#' @return Character vector of the ids of the settings whose stored value was not applied.
#' @noRd
notApplicableSettings <- function(storedInputs, currentInputs, settingIds) {
  isUpload <- function(value) is.data.frame(value) && "datapath" %in% names(value)
  withoutDatapath <- function(value) {
    if (isUpload(value)) value[setdiff(names(value), "datapath")] else value
  }
  applied <- function(stored, current) {
    is.null(stored) ||
      isTRUE(all.equal(withoutDatapath(stored), withoutDatapath(current), check.attributes = FALSE)) &&
        (!isUpload(current) || all(file.exists(current$datapath)))
  }

  ids <- intersect(settingIds, names(storedInputs))
  Filter(function(id) !applied(storedInputs[[id]], currentInputs[[id]]), ids)
}

#' Ignore Stored Settings Which Do Not Fit the Input Data
#'
#' Checks, after the stored settings were restored, whether each of them could be applied.
#' A setting whose stored value is not available anymore (e.g. a track or attribute which is
#' not part of the current input data) keeps what shiny shows instead, e.g. the first choice
#' of a \code{selectInput}, or nothing for a multiple selection, radio buttons or a checkbox
#' group. The warning about not yet stored settings is shown, and the stored settings stay
#' untouched until "Store settings" is clicked. Settings the user already changed are left out.
#'
#' @param session A Shiny session object, typically provided by the Shiny server function.
#' @param settingIds Character vector of the ids of the settings shown in the UI (reported by
#'   the browser once this App finished starting).
#' @param changedIds Character vector of the ids of the settings the user already changed
#'   (reported by the browser together with \code{settingIds}).
#'
#' @return Invisibly, the ids of the stored settings which do not fit the input data (empty if
#'   all fit, or if the stored settings were not restored).
#'
#' @details
#' The check only runs for a session which restored the stored settings (state id "latest").
#' A setting counts as not fitting if the value shown differs from the stored value in any
#' way, also if only parts of a stored selection are available anymore.
#'
#' @examples
#' \dontrun{
#' # In a Shiny server function
#' observeEvent(input$ma_startup_settings, {
#'   startup <- input$ma_startup_settings
#'   ignoreNotApplicableSettings(session, unlist(startup$settingIds), unlist(startup$changedIds))
#' })
#' }
#'
#' @seealso \code{\link{restoreShinyBookmark}} for restoring bookmarks
#' @export
ignoreNotApplicableSettings <- function(session, settingIds, changedIds = character()) {
  tryCatch(
    {
      stateId <- shiny::getQueryString(session)$`_state_id_`
      if (!identical(stateId, "latest") || !fs::file_exists(bookmarkRdsTargetPath)) {
        return(invisible(character()))
      }
      storedInputs <- readRDS(bookmarkRdsTargetPath)
      currentInputs <- shiny::isolate(lapply(settingIds, function(id) session$input[[id]]))
      names(currentInputs) <- settingIds
      ids <- notApplicableSettings(storedInputs, currentInputs, setdiff(settingIds, changedIds))
      if (length(ids) > 0) {
        logger.info(paste("[bookmark] Stored settings which do not fit the input data are ignored:", paste(ids, collapse = ", ")))
        session$sendCustomMessage("ma-settings-not-stored", list())
      }
      invisible(ids)
    },
    error = function(e) {
      logger.error(paste("[bookmark] Could not check whether the stored settings fit the input data:", e))
      invisible(character())
    }
  )
}

#' Finish Starting This App
#'
#' Runs once the browser reports that this App finished starting: all settings are shown,
#' also the ones created via \code{renderUI}, and the App's own updates are applied.
#' Requested default settings are stored only now, so that they are complete. Otherwise,
#' stored settings which do not fit the input data are ignored.
#'
#' @param session A Shiny session object.
#' @param startup The report of the browser (\code{settingIds}, \code{changedIds}).
#' @param defaultsRequested \code{TRUE} if this session started with the default settings
#'   requested via \code{\link{restoreDefaultSettings}}.
#' @param storeSettings Function storing the current settings (also writes \code{input.json});
#'   returns whether this succeeded.
#' @noRd
onStartupFinished <- function(session, startup, defaultsRequested, storeSettings) {
  if (defaultsRequested) {
    # the default settings replace the stored settings (also on MoveApps); `input.json` documents
    # them also if storing failed
    if (!storeSettings()) {
      session$sendCustomMessage("extract-shiny-input", list())
    }
    return(invisible())
  }

  ignoreNotApplicableSettings(session, unlist(startup$settingIds), unlist(startup$changedIds))
  # write `input.json` again: now it also contains the settings created via `renderUI`
  session$sendCustomMessage("extract-shiny-input", list())
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