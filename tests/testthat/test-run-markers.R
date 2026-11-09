test_that("a run stores the result once, with the stored settings, and its render-side artifact is complete at done", {
  skip_on_cran()
  dir <- probeApp(quote({
    output$text <- shiny::renderText({
      writeLines(input$unit, "artifact.txt")
      input$unit
    })
    shiny::reactive(input$unit)
  }))
  # arrange
  app <- shinytest2::AppDriver$new(dir, name = "run-markers-store")
  withr::defer(app$stop())
  # the first session stores after its startup report, the file is the gate
  waitForFile(file.path(dir, "stored.txt"))
  # what the first session left
  file.remove(file.path(dir, c("stored.txt", "artifact-at-done.txt")))
  storeSetting(dir)

  # act
  app$get_chromote_session()$Page$navigate(app$get_url())
  waitForFile(file.path(dir, "stored.txt"))
  # notifyDone follows storeResult
  waitForFile(file.path(dir, "artifact-at-done.txt"))

  # assert
  expect_equal(readLines(file.path(dir, "stored.txt")), "min")
  expect_equal(readLines(file.path(dir, "artifact-at-done.txt")), "min")
})

test_that("a session that ends while opening leaves 'ma-run:disconnected' in the console, the restore reload does not", {
  skip_on_cran()
  dir <- probeApp(quote({
    shiny::observe({
      if (identical(input$unit, "min")) stop("probe: stored setting breaks the App")
    })
    shiny::reactive(input$unit)
  }))
  # arrange
  app <- shinytest2::AppDriver$new(dir, name = "run-markers-console")
  withr::defer(app$stop())
  waitForFile(file.path(dir, "stored.txt"))
  storeSetting(dir)

  # act
  app$get_chromote_session()$Page$navigate(app$get_url())
  # polled from R: `wait_for_js()` ends silently when the page navigates away, as the restore reload does
  waitFor(function() isTRUE(tryCatch(
    app$get_js("!window.maReloading && document.getElementById('shiny-disconnected-overlay') !== null"),
    error = function(e) FALSE
  )))

  # assert
  logs <- app$get_logs()
  # a console line carries its stack trace after the first line
  messages <- sub("\n.*", "", logs$message[logs$location == "chromote"])
  markers <- grep("^ma-run:", messages, value = TRUE)
  expect_equal(markers, c("ma-run:done", "ma-run:disconnected"))
})
