# drives createMoveAppsShinyServer() with shiny's MockShinySession; the mock drops custom messages to
# the browser, so their types are recorded instead
recordingSession <- function() {
  session <- shiny::MockShinySession$new()
  messages <- new.env()
  messages$types <- character()
  session$sendCustomMessage <- function(type, message) messages$types <- c(messages$types, type)
  list(session = session, messages = messages)
}

# the App developer's module, which createMoveAppsShinyServer() looks up globally
defineShinyModule <- function() {
  assign("shinyModule", function(input, output, session, data = NULL) shiny::reactive(NULL), envir = globalenv())
}

# mocks the SDK functions around the wiring under test; records what onStartupFinished() receives
mockServerDependencies <- function(startState, env = parent.frame()) {
  received <- new.env()
  received$defaultsRequested <- logical()
  local_mocked_bindings(
    restoreShinyBookmark = function(session) invisible(startState),
    onStartupFinished = function(session, startup, defaultsRequested, storeSettings) {
      received$defaultsRequested <- c(received$defaultsRequested, defaultsRequested)
    },
    storeResult = function(...) NULL,
    notifyDone = function(...) NULL,
    .package = "moveapps",
    .env = env
  )
  received
}

test_that("createMoveAppsShinyServer stores requested default settings once the start is reported, not at the start", {
  skip_if_not_installed("shiny")
  defineShinyModule()
  on.exit(rm("shinyModule", envir = globalenv()), add = TRUE)
  received <- mockServerDependencies(startState = "defaults")
  mock <- recordingSession()

  shiny::testServer(moveapps::createMoveAppsShinyServer, {
    # the start (testServer does not flush on its own before the first input)
    session$flushReact()
    # nothing stored (a failing store would send "ma-settings-not-stored") and nothing documented yet
    expect_length(mock$messages$types, 0)

    session$setInputs(ma_startup_settings = list(settingIds = list("x"), changedIds = list()))
    expect_equal(received$defaultsRequested, TRUE)
  }, session = mock$session)
})

test_that("createMoveAppsShinyServer documents the settings at the start unless the page is reloading", {
  skip_if_not_installed("shiny")
  defineShinyModule()
  on.exit(rm("shinyModule", envir = globalenv()), add = TRUE)

  expected <- list(none = "extract-shiny-input", reloading = character())
  for (startState in names(expected)) {
    received <- mockServerDependencies(startState = startState)
    mock <- recordingSession()
    shiny::testServer(moveapps::createMoveAppsShinyServer, {
      session$flushReact()
      expect_equal(mock$messages$types, expected[[startState]], label = startState)

      session$setInputs(ma_startup_settings = list(settingIds = list("x"), changedIds = list()))
      expect_equal(received$defaultsRequested, FALSE, label = startState)
    }, session = mock$session)
  }
})

test_that("createMoveAppsShinyServer keeps the SDK's buttons and internal inputs out of the bookmark", {
  skip_if_not_installed("shiny")
  defineShinyModule()
  on.exit(rm("shinyModule", envir = globalenv()), add = TRUE)
  mockServerDependencies(startState = "none")
  mock <- recordingSession()

  shiny::testServer(moveapps::createMoveAppsShinyServer, {
    internal <- c("ma_bookmark", "ma_restore_defaults", "ma_restore_defaults_confirm", "heartbeat", "shiny_input_json", "ma_startup_settings")
    expect_true(all(internal %in% session$getBookmarkExclude()))
  }, session = mock$session)
})

# MockShinySession ignores onBookmarked() and doBookmark(): register the hook and fire it from doBookmark(),
# like shiny does once the bookmark is saved. Returns what the tests inspect.
wireBookmarkHook <- function(session) {
  hooks <- new.env()
  hooks$callback <- NULL
  hooks$queryStrings <- character()
  session$onBookmarked <- function(fun) hooks$callback <- fun
  session$doBookmark <- function() hooks$callback("http://localhost:3838/?_state_id_=abc123")
  session$updateQueryString <- function(queryString, mode) hooks$queryStrings <- c(hooks$queryStrings, queryString)
  hooks
}

# the SDK function storing the settings, handed over to onStartupFinished()
captureStoreSettings <- function(env = parent.frame()) {
  captured <- new.env()
  local_mocked_bindings(
    restoreShinyBookmark = function(session) invisible("reloading"),
    onStartupFinished = function(session, startup, defaultsRequested, storeSettings) captured$storeSettings <- storeSettings,
    storeResult = function(...) NULL,
    notifyDone = function(...) NULL,
    .package = "moveapps",
    .env = env
  )
  captured
}

test_that("createMoveAppsShinyServer counts the settings as stored once the whole bookmark directory is pushed", {
  skip_if_not_installed("shiny")
  defineShinyModule()
  on.exit(rm("shinyModule", envir = globalenv()), add = TRUE)
  captured <- captureStoreSettings()
  pushes <- new.env()
  pushes$dir <- 0
  pushes$single <- character()
  local_mocked_bindings(
    saveBookmarkAsLatest = function(url) invisible(TRUE),
    notifyPushBookmarkDir = function() { pushes$dir <- pushes$dir + 1; invisible(TRUE) },
    notifyPushBookmark = function(fileName) { pushes$single <- c(pushes$single, fileName); invisible(TRUE) },
    .package = "moveapps"
  )
  mock <- recordingSession()
  hooks <- wireBookmarkHook(mock$session)

  shiny::testServer(moveapps::createMoveAppsShinyServer, {
    session$flushReact()
    session$setInputs(ma_startup_settings = list(settingIds = list("x"), changedIds = list()))

    expect_true(captured$storeSettings())
    expect_equal(pushes$dir, 1)
    expect_length(pushes$single, 0)
    expect_equal(hooks$queryStrings, "?_state_id_=latest")
    expect_true("ma-settings-stored" %in% mock$messages$types)
  }, session = mock$session)
})

test_that("createMoveAppsShinyServer does not count the settings as stored if the push fails", {
  skip_if_not_installed("shiny")
  defineShinyModule()
  on.exit(rm("shinyModule", envir = globalenv()), add = TRUE)
  captured <- captureStoreSettings()
  local_mocked_bindings(
    saveBookmarkAsLatest = function(url) invisible(TRUE),
    notifyPushBookmarkDir = function() invisible(FALSE),
    .package = "moveapps"
  )
  mock <- recordingSession()
  wireBookmarkHook(mock$session)

  shiny::testServer(moveapps::createMoveAppsShinyServer, {
    session$flushReact()
    session$setInputs(ma_startup_settings = list(settingIds = list("x"), changedIds = list()))

    expect_false(captured$storeSettings())
    expect_true("ma-settings-not-stored" %in% mock$messages$types)
    expect_false("ma-settings-stored" %in% mock$messages$types)
  }, session = mock$session)
})
