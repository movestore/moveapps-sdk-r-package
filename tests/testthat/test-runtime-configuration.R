test_that("configuration loads JSON and masks settings when requested", {
  old <- Sys.getenv(c("CONFIGURATION","PRINT_CONFIGURATION","MASK_SETTING_IDS","LOG_LEVEL_SDK"))
  on.exit({ do.call(Sys.setenv, as.list(old)); invisible(NULL) }, add = TRUE)

  cfg <- list(url = "http://example.org", secret = "shh", count = 3)
  json <- jsonlite::toJSON(cfg, auto_unbox = TRUE)

  Sys.setenv(CONFIGURATION = json,
             PRINT_CONFIGURATION = "yes",
             MASK_SETTING_IDS = "secret",
             LOG_LEVEL_SDK = "INFO")
  # Ensure logger threshold is INFO so info messages are emitted
  moveapps::logger.init()

  out <- capture.output(res <- moveapps::configuration())
  expect_type(res, "list")
  expect_equal(res$url, cfg$url)
  expect_equal(res$secret, cfg$secret)

  # Printed output should contain masked value, not the real secret
  expect_true(any(grepl("\\*\\*\\*masked\\*\\*\\*", out)))
  expect_false(any(grepl(cfg$secret, out)))
})


test_that("clearRecentOutput resets artifacts directory and deletes output file", {
  old <- Sys.getenv(c("CLEAR_OUTPUT","APP_ARTIFACTS_DIR","OUTPUT_FILE","LOG_LEVEL_SDK"))
  on.exit({ do.call(Sys.setenv, as.list(old)); invisible(NULL) }, add = TRUE)

  artifacts <- file.path(tempdir(), paste0("artifacts-", as.integer(runif(1,1,1e6))))
  dir.create(artifacts)
  # pre-populate with a file
  junk <- file.path(artifacts, "junk.txt")
  writeLines("junk", junk)
  out_file <- tempfile(fileext = ".out")
  writeLines("result", out_file)

  Sys.setenv(CLEAR_OUTPUT = "yes",
             APP_ARTIFACTS_DIR = artifacts,
             OUTPUT_FILE = out_file,
             LOG_LEVEL_SDK = "INFO")
  moveapps::logger.init()

  capture.output(moveapps::clearRecentOutput())

  expect_true(dir.exists(artifacts))
  expect_false(file.exists(junk))
  expect_true(file.exists(file.path(artifacts, ".keep")))
  expect_false(file.exists(out_file))

  # cleanup
  unlink(artifacts, recursive = TRUE)
})
