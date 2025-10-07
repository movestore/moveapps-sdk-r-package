test_that("logger.layout formats messages correctly", {
  msg <- capture.output(moveapps::logger.info("Hello %s %s", "world", NULL))
  # Should include a timestamp, level name padded to 5, message, and newline
  expect_true(endsWith(msg, "[INFO ] Hello world NULL"))
})

test_that("logger.log_level respects threshold", {
  # Save and restore threshold and env var
  old_threshold <- moveapps:::.logger_env$threshold
  old_env <- Sys.getenv("LOG_LEVEL_SDK", unset = NA)
  on.exit({ if (is.na(old_env)) Sys.unsetenv("LOG_LEVEL_SDK") else Sys.setenv(LOG_LEVEL_SDK = old_env) }, add = TRUE)

  # Ensure env var does not override parameter
  Sys.unsetenv("LOG_LEVEL_SDK")
  moveapps::logger.init(threshold_param = "INFO")

  out_info <- capture.output(moveapps::logger.info("info shown"))
  out_warn <- capture.output(moveapps::logger.warn("warn shown"))
  out_debug <- capture.output(moveapps::logger.debug("debug hidden"))
  out_trace <- capture.output(moveapps::logger.trace("trace hidden"))

  expect_true(any(endsWith(out_info, "[INFO ] info shown")))
  expect_true(any(endsWith(out_warn, "[WARN ] warn shown")))
  expect_length(out_debug, 0)
  expect_length(out_trace, 0)
})
