test_that("readRdsInput returns NULL for missing or empty sourceFile", {
  expect_null(moveapps::readRdsInput(NULL))
  expect_null(moveapps::readRdsInput(""))

  tf_empty <- tempfile(fileext = ".rds")
  file.create(tf_empty)
  expect_true(file.exists(tf_empty))
  expect_equal(file.info(tf_empty)$size, 0)
  expect_error(moveapps::readRdsInput(tf_empty), "cannot process NULL-input")
})

test_that("readRdsInput reads valid RDS file", {
  tf <- tempfile(fileext = ".rds")
  obj <- list(a = 1, b = "x")
  saveRDS(obj, tf)
  res <- moveapps::readRdsInput(tf)
  expect_identical(res, obj)
})

test_that("storeRdsOutput writes RDS when result is not NULL", {
  tf <- tempfile(fileext = ".rds")
  obj <- data.frame(x = 1:3)
  on.exit({ if (file.exists(tf)) unlink(tf) }, add = TRUE)
  moveapps::storeRdsOutput(obj, tf)
  expect_true(file.exists(tf))
  read_back <- readRDS(tf)
  expect_identical(read_back, obj)
})

test_that("storeRdsOutput writes empty file when result is NULL", {
  tf <- tempfile(fileext = ".rds")
  on.exit({ if (file.exists(tf)) unlink(tf) }, add = TRUE)
  moveapps::storeRdsOutput(NULL, tf)
  expect_true(file.exists(tf))
  expect_equal(file.info(tf)$size, 0)
})
