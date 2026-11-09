# writes the expression `app` (evaluating to a `shiny::shinyApp()`) as the `app.R` of a temporary app
# directory and returns the directory, removed with the calling test.
# shinytest2 rebuilds a `shinyApp()` object without its `enableBookmarking`, which the bookmark tests
# need: it starts an app directory as it is
writeAppDir <- function(app, env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  writeLines(
    vapply(as.list(app)[-1], function(expr) paste(deparse(expr), collapse = "\n"), character(1)),
    file.path(dir, "app.R")
  )
  dir
}

# stores "min" for the select input, as MoveApps pushes the stored settings into the App
storeSetting <- function(appDir) {
  # the first session already created it, to write `input.json`
  dir.create(file.path(appDir, "shiny_bookmarks", "latest"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(list(`shinyModule-unit` = "min"), file.path(appDir, "shiny_bookmarks", "latest", "input.rds"))
}

# waits for `condition` (a function returning TRUE once met)
waitFor <- function(condition, what = "condition", timeout = 10) {
  deadline <- Sys.time() + timeout
  while (!condition()) {
    if (Sys.time() > deadline) stop("timeout waiting for ", what)
    Sys.sleep(0.1)
  }
}

# waits for `path` to exist, as the App writes it on its own
waitForFile <- function(path, timeout = 10) {
  waitFor(function() file.exists(path), what = path, timeout = timeout)
}

# waits until the browser console holds `n` lines of `marker` ("ma-run:done" follows notifyDone)
waitForMarker <- function(app, marker, n = 1, timeout = 10) {
  waitFor(function() {
    # console events are processed with a browser round trip, not while R sleeps
    tryCatch(app$get_js("0"), error = function(e) NULL)
    logs <- app$get_logs()
    sum(sub("\n.*", "", logs$message[logs$location == "chromote"]) == marker) >= n
  }, what = paste(n, marker), timeout = timeout)
}

# like selectApp(), around the module body `module` (evaluated in `shinyModule(input, output, session, data)`
# and returning the result reactive). Replaces two SDK functions to show what a run delivers: the stored
# result lands in `stored.txt`, the artifact at `notifyDone()` in `artifact-at-done.txt`
probeApp <- function(module, env = parent.frame()) {
  app <- bquote({
    library(moveapps)
    Sys.setenv(HTTP_CLIENT_FAKE = "yes")
    assign("shinyModuleUserInterface", function(id) {
      shiny::tagList(
        shiny::selectInput(shiny::NS(id, "unit"), "Unit", choices = c("s", "min", "h"), selected = "h"),
        shiny::textOutput(shiny::NS(id, "text"))
      )
    }, envir = globalenv())
    assign("shinyModule", function(input, output, session, data = NULL) .(module), envir = globalenv())
    utils::assignInNamespace("storeResult", function(result, outputFile) {
      cat(result, "\n", file = "stored.txt", append = TRUE, sep = "")
    }, ns = "moveapps")
    utils::assignInNamespace("notifyDone", function(executionType) {
      file.copy("artifact.txt", "artifact-at-done.txt", overwrite = TRUE)
    }, ns = "moveapps")
    shiny::shinyApp(createMoveAppsShinyUI, createMoveAppsShinyServer, enableBookmarking = "server")
  })
  writeAppDir(app, env = env)
}
