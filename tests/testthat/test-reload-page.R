# an App with the SDK's UI and server around a module with one select input (default "h")
selectApp <- function(env = parent.frame()) {
  app <- bquote({
    library(moveapps)
    Sys.setenv(HTTP_CLIENT_FAKE = "yes")
    # the functions of the App developer, which the SDK looks up globally (shiny runs `app.R` in its own
    # environment)
    assign("shinyModuleUserInterface", function(id) {
      shiny::selectInput(shiny::NS(id, "unit"), "Unit", choices = c("s", "min", "h"), selected = "h")
    }, envir = globalenv())
    assign("shinyModule", function(input, output, session, data = NULL) shiny::reactive(NULL), envir = globalenv())
    shiny::shinyApp(createMoveAppsShinyUI, createMoveAppsShinyServer, enableBookmarking = "server")
  })
  writeAppDir(app, env = env)
}

# stores "min" for the select input, as MoveApps pushes the stored settings into the App
storeSetting <- function(appDir) {
  # the first session already created it, to write `input.json`
  dir.create(file.path(appDir, "shiny_bookmarks", "latest"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(list(`shinyModule-unit` = "min"), file.path(appDir, "shiny_bookmarks", "latest", "input.rds"))
}

test_that("the stored settings are loaded by a new navigation: Firefox fills a reloaded page with the inputs of the page before", {
  skip_on_cran()
  dir <- selectApp()
  app <- shinytest2::AppDriver$new(dir, name = "reload-page")
  withr::defer(app$stop())
  storeSetting(dir)

  # a new session finds the stored settings, and the SDK loads this page again with them
  app$get_chromote_session()$Page$navigate(app$get_url())
  app$wait_for_js("window.location.search === '?_state_id_=latest' && document.getElementById('shinyModule-unit').value === 'min'")
  actual <- app$get_js("performance.getEntriesByType('navigation')[0].type")

  expect_equal(actual, "navigate")
})
