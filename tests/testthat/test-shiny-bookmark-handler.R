# minimal stand-in for a shiny session, recording query string updates and reloads
mockSession <- function(urlSearch = "", input = list()) {
  calls <- new.env()
  calls$queryStrings <- character()
  calls$reloads <- 0
  session <- list(
    clientData = list(url_search = urlSearch),
    input = input,
    updateQueryString = function(queryString, mode) calls$queryStrings <- c(calls$queryStrings, queryString),
    reload = function() calls$reloads <- calls$reloads + 1
  )
  list(session = session, calls = calls)
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

test_that("saveBookmarkAsLatest removes the outdated copy of the stored settings without the ones which did not fit", {
  old <- setwd(newTempDir())
  on.exit(setwd(old), add = TRUE)
  dir.create("shiny_bookmarks/latestadjusted", recursive = TRUE)
  saveRDS(list(setting = "old"), "shiny_bookmarks/latestadjusted/input.rds")
  dir.create("shiny_bookmarks/abc123", recursive = TRUE)
  saveRDS(list(setting = "new"), "shiny_bookmarks/abc123/input.rds")

  expect_true(moveapps::saveBookmarkAsLatest("http://localhost:3838/?_state_id_=abc123"))
  expect_false(dir.exists("shiny_bookmarks/latestadjusted"))
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

test_that("restoreDefaultSettings deletes the stored settings and reloads with a one-time token", {
  skip_if_not_installed("shiny")
  old <- setwd(newTempDir())
  on.exit(setwd(old), add = TRUE)
  storeFakeBookmark()
  mock <- mockSession("?_state_id_=latest")

  moveapps::restoreDefaultSettings(mock$session)

  expect_false(file.exists("shiny_bookmarks/latest/input.rds"))
  expect_false(file.exists("shiny_bookmarks/latest/input.json"))
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
  # the default settings are stored again after the reload
  storeFakeBookmark()

  reloaded <- mockSession(request$calls$queryStrings)
  expect_true(moveapps::restoreShinyBookmark(reloaded$session))
  expect_equal(reloaded$calls$queryStrings, "?")
  expect_equal(reloaded$calls$reloads, 0)

  # the token can only be used once: the same URL restores the stored settings
  again <- mockSession(request$calls$queryStrings)
  expect_false(moveapps::restoreShinyBookmark(again$session))
  expect_equal(again$calls$reloads, 1)
})

test_that("restoreShinyBookmark ignores a request for the default settings with an unknown token", {
  skip_if_not_installed("shiny")
  old <- setwd(newTempDir())
  on.exit(setwd(old), add = TRUE)
  storeFakeBookmark()
  mock <- mockSession("?_ma_defaults_=unknown")

  expect_false(moveapps::restoreShinyBookmark(mock$session))
  expect_equal(mock$calls$queryStrings, c("?", "?_state_id_=latest"))
  expect_equal(mock$calls$reloads, 1)
  expect_true(file.exists("shiny_bookmarks/latest/input.rds"))
})

test_that("restoreShinyBookmark reloads with the stored settings only if they exist and are not loaded yet", {
  skip_if_not_installed("shiny")
  old <- setwd(newTempDir())
  on.exit(setwd(old), add = TRUE)

  nothingStored <- mockSession()
  expect_false(moveapps::restoreShinyBookmark(nothingStored$session))
  expect_equal(nothingStored$calls$reloads, 0)

  storeFakeBookmark()
  notLoaded <- mockSession()
  expect_false(moveapps::restoreShinyBookmark(notLoaded$session))
  expect_equal(notLoaded$calls$queryStrings, "?_state_id_=latest")
  expect_equal(notLoaded$calls$reloads, 1)

  alreadyLoaded <- mockSession("?_state_id_=latest")
  expect_false(moveapps::restoreShinyBookmark(alreadyLoaded$session))
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

test_that("ignoreNotApplicableSettings reloads with a copy of the stored settings without the ones which do not fit", {
  skip_if_not_installed("shiny")
  old <- setwd(newTempDir())
  on.exit(setwd(old), add = TRUE)
  dir.create("shiny_bookmarks/latest", recursive = TRUE)
  stored <- list(animal = "C", speed = 90L, width = 3L)
  saveRDS(stored, "shiny_bookmarks/latest/input.rds")
  mock <- mockSession("?_state_id_=latest", input = list(animal = "A", speed = 90L, width = 3L))

  expect_true(moveapps::ignoreNotApplicableSettings(mock$session, c("animal", "speed", "width")))
  expect_identical(readRDS("shiny_bookmarks/latestadjusted/input.rds"), list(speed = 90L, width = 3L))
  expect_identical(readRDS("shiny_bookmarks/latest/input.rds"), stored)
  expect_equal(mock$calls$queryStrings, "?_state_id_=latestadjusted")
  expect_equal(mock$calls$reloads, 1)
})

test_that("ignoreNotApplicableSettings does nothing if all stored settings fit or none were restored", {
  skip_if_not_installed("shiny")
  old <- setwd(newTempDir())
  on.exit(setwd(old), add = TRUE)
  dir.create("shiny_bookmarks/latest", recursive = TRUE)
  saveRDS(list(animal = "C"), "shiny_bookmarks/latest/input.rds")

  allFit <- mockSession("?_state_id_=latest", input = list(animal = "C"))
  expect_false(moveapps::ignoreNotApplicableSettings(allFit$session, "animal"))
  expect_equal(allFit$calls$reloads, 0)

  # not restored from the stored settings (e.g. default settings, or already reloaded without the ones which do not fit)
  for (urlSearch in c("", "?_state_id_=latestadjusted")) {
    notRestored <- mockSession(urlSearch, input = list(animal = "A"))
    expect_false(moveapps::ignoreNotApplicableSettings(notRestored$session, "animal"))
    expect_equal(notRestored$calls$reloads, 0)
  }
  expect_false(dir.exists("shiny_bookmarks/latestadjusted"))
})
