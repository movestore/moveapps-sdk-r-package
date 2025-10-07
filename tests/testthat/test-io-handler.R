test_that("sourceFile, outputFile, errorFile return env vars or empty string", {
  old <- Sys.getenv(c("SOURCE_FILE","OUTPUT_FILE","ERROR_FILE"))
  on.exit({ do.call(Sys.setenv, as.list(old)); invisible(NULL) }, add = TRUE)

  # Defaults
  Sys.unsetenv(c("SOURCE_FILE","OUTPUT_FILE","ERROR_FILE"))
  expect_equal(moveapps::sourceFile(), "")
  expect_equal(moveapps::outputFile(), "")
  expect_equal(moveapps::errorFile(), "")

  # Set values
  Sys.setenv(SOURCE_FILE = "/tmp/source.rds",
             OUTPUT_FILE = "/tmp/output.rds",
             ERROR_FILE = "/tmp/error.log")
  expect_equal(moveapps::sourceFile(), "/tmp/source.rds")
  expect_equal(moveapps::outputFile(), "/tmp/output.rds")
  expect_equal(moveapps::errorFile(), "/tmp/error.log")
})

