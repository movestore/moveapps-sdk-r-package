# an App with the SDK's UI and server around a module with an upload
unsavedSettingsApp <- function(env = parent.frame()) {
  app <- bquote({
    library(moveapps)
    Sys.setenv(HTTP_CLIENT_FAKE = "yes")
    # the functions of the App developer, which the SDK looks up globally (shiny runs `app.R` in its own
    # environment)
    assign("shinyModuleUserInterface", function(id) {
      shiny::fileInput(shiny::NS(id, "upload"), "Upload")
    }, envir = globalenv())
    assign("shinyModule", function(input, output, session, data = NULL) shiny::reactive(NULL), envir = globalenv())
    shiny::shinyApp(createMoveAppsShinyUI, createMoveAppsShinyServer, enableBookmarking = "server")
  })
  writeAppDir(app, env = env)
}

# whether the warning about not yet stored settings is shown
warningShown <- function(app) {
  app$get_js("document.getElementById('ma_unsaved_settings').style.display !== 'none'")
}

# a real mouse press: the warning ignores changes until the user interacted with the page (before
# that, they are part of the start of the App)
userInteracts <- function(app) {
  app$get_chromote_session()$Input$dispatchMouseEvent(type = "mousePressed", x = 5, y = 5, button = "left", clickCount = 1)
  app$get_chromote_session()$Input$dispatchMouseEvent(type = "mouseReleased", x = 5, y = 5, button = "left", clickCount = 1)
}

test_that("the warning about not yet stored settings follows an upload and disappears when the settings are stored", {
  skip_on_cran()
  app <- shinytest2::AppDriver$new(unsavedSettingsApp(), name = "unsaved-settings-warning")
  withr::defer(app$stop())
  path <- file.path(tempdir(), "area.gpkg")
  writeLines("polygon", path)
  app$wait_for_idle(duration = 2500)
  userInteracts(app)
  # control: nothing changed yet
  expect_false(warningShown(app))

  app$upload_file("shinyModule-upload" = path)
  app$wait_for_js("document.getElementById('ma_unsaved_settings').style.display !== 'none'")
  expect_true(warningShown(app))

  app$click(selector = "#ma_bookmark")
  app$wait_for_idle()
  expect_false(warningShown(app))
})
