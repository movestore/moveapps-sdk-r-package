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
