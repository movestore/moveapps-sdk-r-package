# the App developer's function, which runMoveAppsApp() looks up globally; records the arguments it receives
defineRecordingRFunction <- function(env = parent.frame()) {
  received <- new.env()
  assign("rFunction", function(...) {
    received$args <- list(...)
    NULL
  }, envir = globalenv())
  withr::defer(rm("rFunction", envir = globalenv()), envir = env)
  received
}

# the environment runMoveAppsApp() reads its configuration, input and output from
localRunEnvironment <- function(configuration, sourceFile = "", env = parent.frame()) {
  withr::local_envvar(
    CONFIGURATION = jsonlite::toJSON(configuration, auto_unbox = TRUE),
    SOURCE_FILE = sourceFile,
    OUTPUT_FILE = withr::local_tempfile(.local_envir = env),
    ERROR_FILE = "",
    .local_envir = env
  )
}

test_that("runMoveAppsApp hands the configuration to rFunction", {
  # arrange
  localRunEnvironment(configuration = list(username = "secret", window = 3))
  received <- defineRecordingRFunction()

  # act
  moveapps::runMoveAppsApp()

  # assert
  expect_equal(received$args, list(username = "secret", window = 3))
})

test_that("runMoveAppsApp hands given settings to rFunction instead of the configuration", {
  # arrange
  localRunEnvironment(configuration = list(username = "secret", window = 3))
  received <- defineRecordingRFunction()
  args <- moveapps::configuration()
  args[["username"]] <- "personal"

  # act
  moveapps::runMoveAppsApp(args)

  # assert
  expect_equal(received$args, list(username = "personal", window = 3))
})

test_that("runMoveAppsApp stores a broken configuration to the error file", {
  # arrange
  errorFile <- withr::local_tempfile()
  withr::local_envvar(CONFIGURATION = "{broken", SOURCE_FILE = "", OUTPUT_FILE = "", ERROR_FILE = errorFile)
  received <- defineRecordingRFunction()

  # act
  error <- tryCatch(capture.output(moveapps::runMoveAppsApp()), error = function(e) e)

  # assert
  expect_s3_class(error, "error")
  expect_gt(file.size(errorFile), 0)
  expect_null(received$args)
})

test_that("runMoveAppsApp adds the input data to given settings", {
  # arrange
  input <- withr::local_tempfile(fileext = ".rds")
  saveRDS(data.frame(x = 1:2), input)
  localRunEnvironment(configuration = list(), sourceFile = input)
  received <- defineRecordingRFunction()

  # act
  moveapps::runMoveAppsApp(list(username = "personal"))

  # assert
  expect_equal(received$args, list(username = "personal", data = data.frame(x = 1:2)))
})
