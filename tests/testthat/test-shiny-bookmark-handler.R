# minimal stand-in for a shiny session, recording query string updates, reloads and custom messages
mockSession <- function(urlSearch = "", input = list()) {
  calls <- new.env()
  calls$queryStrings <- character()
  calls$reloads <- 0
  calls$messages <- character()
  session <- list(
    clientData = list(url_search = urlSearch),
    input = input,
    updateQueryString = function(queryString, mode) calls$queryStrings <- c(calls$queryStrings, queryString),
    reload = function() calls$reloads <- calls$reloads + 1,
    sendCustomMessage = function(type, message) calls$messages <- c(calls$messages, type)
  )
  list(session = session, calls = calls)
}

# stand-in for storing the settings (bookmark and upload), counting its calls
fakeStoreSettings <- function(succeeds) {
  calls <- new.env()
  calls$count <- 0
  list(fn = function() { calls$count <- calls$count + 1; succeeds }, calls = calls)
}

# empty temporary directory to run the bookmark functions (which use relative paths) in
newTempDir <- function() {
  dir <- tempfile("bookmarks")
  dir.create(dir)
  dir
}

storeFakeBookmark <- function() {
  dir.create("shiny_bookmarks/latest", recursive = TRUE, showWarnings = FALSE)
  saveRDS(list(setting = "stored"), "shiny_bookmarks/latest/input.rds")
  writeLines("[]", "shiny_bookmarks/latest/input.json")
}

test_that("saveBookmarkAsLatest moves the bookmark and returns TRUE", {
  old <- setwd(newTempDir())
  on.exit(setwd(old), add = TRUE)
  dir.create("shiny_bookmarks/abc123", recursive = TRUE)
  saveRDS(list(setting = "new"), "shiny_bookmarks/abc123/input.rds")

  expect_true(moveapps::saveBookmarkAsLatest("http://localhost:3838/?_state_id_=abc123"))
  expect_identical(readRDS("shiny_bookmarks/latest/input.rds"), list(setting = "new"))
  expect_false(dir.exists("shiny_bookmarks/abc123"))
})

test_that("saveBookmarkAsLatest returns FALSE if the bookmark does not exist", {
  old <- setwd(newTempDir())
  on.exit(setwd(old), add = TRUE)
  expect_false(moveapps::saveBookmarkAsLatest("http://localhost:3838/?_state_id_=missing"))
})

test_that("notifyPushBookmark returns TRUE if uploading is faked", {
  old <- Sys.getenv("HTTP_CLIENT_FAKE", unset = NA)
  on.exit({ if (is.na(old)) Sys.unsetenv("HTTP_CLIENT_FAKE") else Sys.setenv(HTTP_CLIENT_FAKE = old) }, add = TRUE)
  Sys.setenv(HTTP_CLIENT_FAKE = "yes")
  expect_true(moveapps::notifyPushBookmark("input.rds"))
})

test_that("restoreDefaultSettings reloads with a one-time token and keeps the stored settings until the defaults are stored", {
  skip_if_not_installed("shiny")
  old <- setwd(newTempDir())
  on.exit(setwd(old), add = TRUE)
  storeFakeBookmark()
  mock <- mockSession("?_state_id_=latest")

  moveapps::restoreDefaultSettings(mock$session)

  # MoveApps keeps its copy until the defaults are stored: deleting the local one would get out of sync
  expect_true(file.exists("shiny_bookmarks/latest/input.rds"))
  expect_true(file.exists("shiny_bookmarks/latest/input.json"))
  expect_equal(mock$calls$reloads, 1)
  expect_length(mock$calls$queryStrings, 1)
  expect_match(mock$calls$queryStrings, "^\\?_ma_defaults_=.+")
})

test_that("restoreDefaultSettings works without stored settings", {
  skip_if_not_installed("shiny")
  old <- setwd(newTempDir())
  on.exit(setwd(old), add = TRUE)
  mock <- mockSession()

  moveapps::restoreDefaultSettings(mock$session)

  expect_equal(mock$calls$reloads, 1)
})

test_that("restoreShinyBookmark skips the restore once for a requested reload with the default settings", {
  skip_if_not_installed("shiny")
  old <- setwd(newTempDir())
  on.exit(setwd(old), add = TRUE)
  storeFakeBookmark()
  request <- mockSession()
  moveapps::restoreDefaultSettings(request$session)

  reloaded <- mockSession(request$calls$queryStrings)
  expect_equal(moveapps::restoreShinyBookmark(reloaded$session), "defaults")
  expect_equal(reloaded$calls$queryStrings, "?")
  expect_equal(reloaded$calls$reloads, 0)

  # the token can only be used once: the same URL restores the stored settings
  again <- mockSession(request$calls$queryStrings)
  expect_equal(moveapps::restoreShinyBookmark(again$session), "reloading")
  expect_equal(again$calls$reloads, 1)
})

test_that("restoreShinyBookmark ignores a request for the default settings with an unknown token", {
  skip_if_not_installed("shiny")
  old <- setwd(newTempDir())
  on.exit(setwd(old), add = TRUE)
  storeFakeBookmark()
  mock <- mockSession("?_ma_defaults_=unknown")

  expect_equal(moveapps::restoreShinyBookmark(mock$session), "reloading")
  expect_equal(mock$calls$queryStrings, c("?", "?_state_id_=latest"))
  expect_equal(mock$calls$reloads, 1)
  expect_true(file.exists("shiny_bookmarks/latest/input.rds"))
})

test_that("restoreShinyBookmark reloads with the stored settings only if they exist and are not loaded yet", {
  skip_if_not_installed("shiny")
  old <- setwd(newTempDir())
  on.exit(setwd(old), add = TRUE)

  nothingStored <- mockSession()
  expect_equal(moveapps::restoreShinyBookmark(nothingStored$session), "none")
  expect_equal(nothingStored$calls$reloads, 0)

  storeFakeBookmark()
  notLoaded <- mockSession()
  expect_equal(moveapps::restoreShinyBookmark(notLoaded$session), "reloading")
  expect_equal(notLoaded$calls$queryStrings, "?_state_id_=latest")
  expect_equal(notLoaded$calls$reloads, 1)

  alreadyLoaded <- mockSession("?_state_id_=latest")
  expect_equal(moveapps::restoreShinyBookmark(alreadyLoaded$session), "none")
  expect_equal(alreadyLoaded$calls$reloads, 0)
})

test_that("notApplicableSettings finds the stored settings which were not applied", {
  stored <- list(animal = "C", animals = c("A", "C"), speed = 90L, width = 3L, hidden = "x")
  current <- list(animal = "A", animals = "A", speed = 50, width = 3, hidden = "y")
  settingIds <- c("animal", "animals", "speed", "width", "not_stored")

  # a partly available selection does not fit either; integer and double of the same value fit;
  # inputs which are no settings (not in `settingIds`) and settings which were not stored are ignored
  expect_equal(moveapps:::notApplicableSettings(stored, current, settingIds), c("animal", "animals", "speed"))
  expect_length(moveapps:::notApplicableSettings(stored, stored, settingIds), 0)
})

test_that("notApplicableSettings takes a restored upload as applied although shiny moved it to a temporary path", {
  upload <- function(name, datapath) data.frame(name = name, size = 17L, type = "application/octet-stream", datapath = datapath)
  stored <- list(upload = upload("area.gpkg", "0.gpkg"))
  # shiny copies a restored upload into a new temporary dir and hands the app that path
  restoredPath <- file.path(newTempDir(), "0.gpkg")
  writeLines("polygon", restoredPath)
  restored <- list(upload = upload("area.gpkg", restoredPath))

  expect_length(moveapps:::notApplicableSettings(stored, restored, "upload"), 0)
  # control: uploads are still compared, a different file does not fit
  expect_equal(moveapps:::notApplicableSettings(stored, list(upload = upload("other.gpkg", restoredPath)), "upload"), "upload")
})

test_that("notApplicableSettings reports a restored upload whose file is missing", {
  upload <- function(datapath) data.frame(name = "area.gpkg", size = 17L, type = "application/octet-stream", datapath = datapath)
  stored <- list(upload = upload("0.gpkg"))
  # the bookmark did not keep the file: shiny's copy fails silently and hands the app a path to nothing
  restored <- list(upload = upload(file.path(newTempDir(), "0.gpkg")))

  expect_equal(moveapps:::notApplicableSettings(stored, restored, "upload"), "upload")
})

test_that("ignoreNotApplicableSettings shows the warning and keeps the session when stored settings do not fit", {
  skip_if_not_installed("shiny")
  old <- setwd(newTempDir())
  on.exit(setwd(old), add = TRUE)
  dir.create("shiny_bookmarks/latest", recursive = TRUE)
  stored <- list(animal = "C", speed = 90L, width = 3L)
  saveRDS(stored, "shiny_bookmarks/latest/input.rds")
  # "C" is not part of the data anymore: shiny shows its first choice instead
  mock <- mockSession("?_state_id_=latest", input = list(animal = "A", speed = 90L, width = 3L))

  expect_equal(moveapps::ignoreNotApplicableSettings(mock$session, c("animal", "speed", "width")), "animal")
  # no reload and no copy of the stored settings: they stay untouched until the user stores again
  expect_equal(mock$calls$reloads, 0)
  expect_equal(mock$calls$messages, "ma-settings-not-stored")
  expect_identical(readRDS("shiny_bookmarks/latest/input.rds"), stored)
  expect_equal(list.files("shiny_bookmarks"), "latest")
})

test_that("ignoreNotApplicableSettings does nothing if all stored settings fit or none were restored", {
  skip_if_not_installed("shiny")
  old <- setwd(newTempDir())
  on.exit(setwd(old), add = TRUE)
  dir.create("shiny_bookmarks/latest", recursive = TRUE)
  saveRDS(list(animal = "C"), "shiny_bookmarks/latest/input.rds")

  allFit <- mockSession("?_state_id_=latest", input = list(animal = "C"))
  expect_length(moveapps::ignoreNotApplicableSettings(allFit$session, "animal"), 0)
  expect_length(allFit$calls$messages, 0)

  # not restored from the stored settings, e.g. the default settings
  notRestored <- mockSession("", input = list(animal = "A"))
  expect_length(moveapps::ignoreNotApplicableSettings(notRestored$session, "animal"), 0)
  expect_length(notRestored$calls$messages, 0)
})

test_that("ignoreNotApplicableSettings leaves out the settings the user already changed", {
  skip_if_not_installed("shiny")
  old <- setwd(newTempDir())
  on.exit(setwd(old), add = TRUE)
  dir.create("shiny_bookmarks/latest", recursive = TRUE)
  saveRDS(list(animal = "C", speed = 90L), "shiny_bookmarks/latest/input.rds")
  # the user picked "A" themselves; the stored speed fits
  mock <- mockSession("?_state_id_=latest", input = list(animal = "A", speed = 90L))

  expect_length(moveapps::ignoreNotApplicableSettings(mock$session, c("animal", "speed"), changedIds = "animal"), 0)
  expect_length(mock$calls$messages, 0)
})

test_that("ignoreNotApplicableSettings still checks the other settings once the user changed some", {
  skip_if_not_installed("shiny")
  old <- setwd(newTempDir())
  on.exit(setwd(old), add = TRUE)
  dir.create("shiny_bookmarks/latest", recursive = TRUE)
  saveRDS(list(animal = "C", speed = 90L), "shiny_bookmarks/latest/input.rds")
  # "C" is not part of the data anymore; the user already changed the speed
  mock <- mockSession("?_state_id_=latest", input = list(animal = "A", speed = 50L))

  expect_equal(moveapps::ignoreNotApplicableSettings(mock$session, c("animal", "speed"), changedIds = "speed"), "animal")
  expect_equal(mock$calls$messages, "ma-settings-not-stored")
})

test_that("onStartupFinished leaves out the settings the user changed during the start", {
  skip_if_not_installed("shiny")
  old <- setwd(newTempDir())
  on.exit(setwd(old), add = TRUE)
  dir.create("shiny_bookmarks/latest", recursive = TRUE)
  saveRDS(list(animal = "C"), "shiny_bookmarks/latest/input.rds")
  mock <- mockSession("?_state_id_=latest", input = list(animal = "A"))
  store <- fakeStoreSettings(succeeds = TRUE)

  startup <- list(settingIds = list("animal"), changedIds = list("animal"))
  moveapps:::onStartupFinished(mock$session, startup, defaultsRequested = FALSE, storeSettings = store$fn)

  expect_equal(mock$calls$reloads, 0)
  expect_equal(mock$calls$messages, "extract-shiny-input")
})

test_that("onStartupFinished stores the requested default settings once this App finished starting", {
  skip_if_not_installed("shiny")
  old <- setwd(newTempDir())
  on.exit(setwd(old), add = TRUE)
  mock <- mockSession()
  store <- fakeStoreSettings(succeeds = TRUE)

  moveapps:::onStartupFinished(mock$session, list(settingIds = list("animal")), defaultsRequested = TRUE, storeSettings = store$fn)

  expect_equal(store$calls$count, 1)
  # a successful store writes `input.json` itself
  expect_length(mock$calls$messages, 0)
})

test_that("onStartupFinished writes input.json although storing the default settings failed", {
  skip_if_not_installed("shiny")
  old <- setwd(newTempDir())
  on.exit(setwd(old), add = TRUE)
  mock <- mockSession()
  store <- fakeStoreSettings(succeeds = FALSE)

  moveapps:::onStartupFinished(mock$session, list(settingIds = list("animal")), defaultsRequested = TRUE, storeSettings = store$fn)

  expect_equal(store$calls$count, 1)
  expect_equal(mock$calls$messages, "extract-shiny-input")
})

test_that("onStartupFinished writes input.json and stores nothing if no default settings were requested", {
  skip_if_not_installed("shiny")
  old <- setwd(newTempDir())
  on.exit(setwd(old), add = TRUE)
  mock <- mockSession()
  store <- fakeStoreSettings(succeeds = TRUE)

  moveapps:::onStartupFinished(mock$session, list(settingIds = list("animal")), defaultsRequested = FALSE, storeSettings = store$fn)

  expect_equal(store$calls$count, 0)
  expect_equal(mock$calls$messages, "extract-shiny-input")
})

test_that("onStartupFinished shows the warning for stored settings which do not fit and writes input.json", {
  skip_if_not_installed("shiny")
  old <- setwd(newTempDir())
  on.exit(setwd(old), add = TRUE)
  dir.create("shiny_bookmarks/latest", recursive = TRUE)
  saveRDS(list(animal = "C"), "shiny_bookmarks/latest/input.rds")
  mock <- mockSession("?_state_id_=latest", input = list(animal = "A"))
  store <- fakeStoreSettings(succeeds = TRUE)

  moveapps:::onStartupFinished(mock$session, list(settingIds = list("animal")), defaultsRequested = FALSE, storeSettings = store$fn)

  # `input.json` documents the shown settings, which this App uses
  expect_equal(mock$calls$reloads, 0)
  expect_equal(mock$calls$messages, c("ma-settings-not-stored", "extract-shiny-input"))
})
