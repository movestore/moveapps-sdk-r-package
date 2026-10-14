# CHANGELOG

All notable changes to the MoveApps R SDK will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [v1.2.0] - 2026-10-13

### Added
- `runMoveAppsApp(args)` runs the App with the given settings instead of the configuration, e.g. with personal credentials during local development; `runMoveAppsApp()` without arguments loads the configuration as before

### Fixed
- In Firefox, a `selectInput()` (shiny) showed its default instead of the stored setting, and "Restore default settings" kept the old value of a `selectInput()` or of an input changed on the page, and stored it as the default: Firefox fills a reloaded page with the former selection of every select and the former value of every changed input. The SDK now loads the page anew instead of reloading it

## [v1.1.0] - 2026-09-30

### Added
- "Restore default settings" (shiny) button next to the "Store settings" button, which (after confirmation) reloads this App with its default settings and stores these, replacing the stored settings (new exported function `restoreDefaultSettings()`)
- Warning next to the "Store settings" (shiny) button while changed settings are not stored yet, or storing them failed
- Stored settings which do not fit the current input data (e.g. a track or attribute created via `renderUI` which is not part of the data anymore) are ignored: such a setting shows what shiny makes of the stored value (e.g. the first choice of a `selectInput()`, or nothing for a multiple selection, radio buttons or a checkbox group), together with the warning about not yet stored settings (new exported function `ignoreNotApplicableSettings()`)
- `notifyPushBookmarkDir()`: pushes the whole shiny bookmark (`input.rds`, `input.json` and the files stored next to them) to MoveApps

### Changed
- `saveBookmarkAsLatest()` keeps every file shiny stores in the bookmark (e.g. the uploads of a `fileInput()`), not only `input.rds`, and removes the files of an earlier bookmark it does not carry anymore
- `saveBookmarkAsLatest()` and `notifyPushBookmark()` return (invisibly) whether the bookmark was saved/uploaded
- The shiny bookmark is only uploaded if it was saved successfully
- "Store settings" (shiny) pushes the whole bookmark instead of `input.rds` only, so the uploads of a `fileInput()` are available in future runs of the workflow (needs the pilot route `/copilot/shiny-bookmark-dir`)
- The WebSocket heartbeat and the JSON extraction are no longer stored in the shiny bookmark
- `input.json` is written (and uploaded) again once the (shiny) App finished starting, so that it also contains the settings created via `renderUI`
- A stored upload whose file is missing (e.g. in a shared workflow, which gets the settings but not the files) is dropped before the settings are restored, instead of handing the App a path to nothing
- A stored multi-file upload, and the uploads of two `fileInput()`s sharing one stored file name, are dropped too (shiny cannot restore them correctly)
- `input.json` records every setting by its input id and the value shiny sends to the server, e.g. all selected options of a checkbox group and both dates of a date range (#7); radio buttons and uploads also carry the text the user saw (`valueLabel`), e.g. the names of the uploaded files; an empty setting (e.g. an upload not made yet) is recorded as `""`, and a checkbox keeps its label

## [v1.0.3] - 2025-12-16

### Changed
- Fixed bug where top app padding would change after first `ws-heartbeat` ping

## [v1.0.2] - 2025-12-04

### Changed
- Mask sensitive values in `bookmark.json`
- Improved logging about non present auxilary file
- Place the "Store settings" (shiny) button at the top end for better accessibility

## [v1.0.1] - 2025-10-14

### Changed
- Removed debug log messages which might be misleading for production environments

## [v1.0.0] - 2025-10-08

### Added
- Initial release of the MoveApps R package
- Centralized SDK code from the three template projects ([R-App](https://github.com/movestore/Template_R_Function_App), [R-Shiny-App](https://github.com/movestore/Template_R_Shiny_App), [R-Shiny-Dashboard-App](https://github.com/movestore/Template_R_Shinydashboard_App)) for improved maintainability and simplified templates
- Unified codebase for both local app development and production environments

### Changed
- Backward compatibility: Existing apps can update to the new template versions (except for deprecated features that have been removed)

### Removed
- Deprecated app setting type `LOCAL_FILE` and its associated function `getAppFilePath()`