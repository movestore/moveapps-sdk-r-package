# a page with the SDK's scripts and one input of each kind from SDK issue #7, as an app directory
extractInputsApp <- function(env = parent.frame()) {
  app <- bquote({
    # a function, so that shiny restores the inputs (e.g. an upload) into the page of a bookmark
    ui <- function(request) shiny::fluidPage(
      shiny::includeScript(.(system.file("shiny-apps/www/setting-values.js", package = "moveapps"))),
      shiny::includeScript(.(system.file("shiny-apps/www/extract-inputs.js", package = "moveapps"))),
      shiny::checkboxGroupInput(inputId = "cbg", label = "Checkbox group", choices = c("A", "B", "C"), selected = c("B", "C")),
      shiny::radioButtons(inputId = "radio3", label = "Radio (3 choices)", choices = c("Choice x" = "x", "Choice y" = "y", "Choice z" = "z"), selected = "y"),
      shiny::dateRangeInput(inputId = "dr", label = "Date range", start = "2024-01-01", end = "2024-12-31"),
      shiny::dateInput(inputId = "d", label = "Date", value = "2024-05-01"),
      shiny::selectInput(inputId = "sel_multi", label = "Select multiple", choices = c("A", "B", "C"), selected = c("A", "C"), multiple = TRUE),
      shiny::passwordInput(inputId = "pw", label = "Password", value = "secret"),
      shiny::checkboxGroupInput(inputId = "cbg_nolabel", label = NULL, choices = c("A", "B"), selected = "B"),
      shiny::radioButtons(inputId = "radio1", label = "Radio (1 choice)", choices = c("only")),
      shiny::sliderInput(inputId = "range", label = "Slider range", min = 0, max = 100, value = c(10, 90)),
      shiny::checkboxInput(inputId = "cb1", label = "Checkbox one", value = TRUE),
      shiny::checkboxInput(inputId = "cb2", label = "Checkbox two"),
      shiny::textInput(inputId = "txt", label = "Text", value = "hello"),
      shiny::numericInput(inputId = "num_empty", label = "Numeric empty", value = NA),
      shiny::passwordInput(inputId = "pw_empty", label = "Password empty", value = ""),
      shiny::fileInput(inputId = "upload", label = "Upload"),
      shiny::actionButton(inputId = "extract", label = "Extract"),
      shiny::bookmarkButton(id = "bookmark")
    )
    server <- function(input, output, session) {
      bookmarkUrl <- shiny::reactiveVal(NULL)
      shiny::setBookmarkExclude(c("extract", "bookmark"))
      shiny::onBookmarked(function(url) bookmarkUrl(url))
      shiny::observeEvent(input$extract, session$sendCustomMessage("extract-shiny-input", list()))
      # shiny wires only the bookmark button with its default id
      shiny::observeEvent(input$bookmark, session$doBookmark())
      shiny::exportTestValues(json = input$shiny_input_json, bookmarkUrl = bookmarkUrl())
    }
    shiny::shinyApp(ui, server, enableBookmarking = "server")
  })
  writeAppDir(app, env = env)
}

# the settings `extract-inputs.js` records, by input id
extractedSettings <- function(app) {
  app$click("extract")
  app$wait_for_value(export = "json", ignore = list(NULL))
  entries <- jsonlite::fromJSON(app$get_value(export = "json"), simplifyVector = FALSE)
  stats::setNames(entries, vapply(entries, function(entry) entry$inputId, character(1)))
}

newApp <- function() {
  skip_on_cran()
  app <- shinytest2::AppDriver$new(extractInputsApp(env = parent.frame()), name = "extract-inputs")
  withr::defer(app$stop(), envir = parent.frame())
  app
}

test_that("extract-inputs.js records every setting by its input id and complete value", {
  app <- newApp()

  actual <- extractedSettings(app)

  expect_equal(actual$cbg$label, "Checkbox group")
  expect_equal(unlist(actual$cbg$value), c("B", "C"))
  expect_equal(actual$radio3$value, "y")
  expect_equal(actual$radio3$valueLabel, "Choice y")
  expect_equal(unlist(actual$dr$value), c("2024-01-01", "2024-12-31"))
  expect_equal(actual$d$value, "2024-05-01")
  expect_equal(unlist(actual$sel_multi$value), c("A", "C"))
  expect_equal(actual$pw$value, "***masked***")
  expect_equal(unlist(actual$cbg_nolabel$value), "B")
  expect_equal(actual$cbg_nolabel$label, "")
  expect_equal(actual$radio1$value, "only")
  expect_equal(actual$radio1$valueLabel, "only")
  expect_equal(unlist(actual$range$value), c(10, 90))
  expect_equal(actual$cb1$label, "Checkbox one")
  expect_equal(actual$cb1$value, TRUE)
  expect_equal(actual$cb2$label, "Checkbox two")
  expect_equal(actual$cb2$value, FALSE)
  expect_equal(actual$txt$value, "hello")
  # groundcontrol requires a value: an empty setting is "", never null
  expect_equal(actual$num_empty$value, "")
  expect_equal(actual$pw_empty$value, "")
  expect_equal(actual$upload$value, "")
  expect_null(actual$upload$valueLabel)
  # control: an action button is no setting
  expect_null(actual$extract)
})

test_that("extract-inputs.js records the name of a freshly uploaded file", {
  app <- newApp()
  path <- file.path(tempdir(), "area.gpkg")
  writeLines("polygon", path)

  app$upload_file(upload = path)
  actual <- extractedSettings(app)

  expect_equal(actual$upload$valueLabel, "area.gpkg")
})

test_that("extract-inputs.js records the name of a restored upload", {
  app <- newApp()
  path <- file.path(tempdir(), "area.gpkg")
  writeLines("polygon", path)
  app$upload_file(upload = path)
  app$click("bookmark")
  app$wait_for_value(export = "bookmarkUrl", ignore = list(NULL))

  # a restored session: the browser never had the file, shiny knows it from the bookmark only. A
  # second driver opens it, as navigating the first one drops shinytest2's hooks from the page
  restored <- shinytest2::AppDriver$new(app$get_value(export = "bookmarkUrl"), name = "extract-inputs-restored")
  withr::defer(restored$stop())
  actual <- extractedSettings(restored)

  expect_equal(actual$upload$valueLabel, "area.gpkg")
})
