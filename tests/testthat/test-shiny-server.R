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
mockServerDependencies <- function(startState, showsIgnored = FALSE, env = parent.frame()) {
  received <- new.env()
  received$defaultsRequested <- logical()
  local_mocked_bindings(
    restoreShinyBookmark = function(session) invisible(startState),
    showsIgnoredSettings = function(session) showsIgnored,
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

test_that("createMoveAppsShinyServer shows the warning in a session reloaded without the settings which do not fit", {
  skip_if_not_installed("shiny")
  defineShinyModule()
  on.exit(rm("shinyModule", envir = globalenv()), add = TRUE)
  mockServerDependencies(startState = "none", showsIgnored = TRUE)
  mock <- recordingSession()

  shiny::testServer(moveapps::createMoveAppsShinyServer, {
    session$flushReact()
    expect_true("ma-settings-not-stored" %in% mock$messages$types)
  }, session = mock$session)
})
