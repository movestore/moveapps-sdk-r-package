# CHANGELOG

All notable changes to the MoveApps R SDK will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- "Restore default settings" (shiny) button next to the "Store settings" button, which (after confirmation) reloads this App with its default settings and stores these, replacing the stored settings (new exported function `restoreDefaultSettings()`)
- Warning next to the "Store settings" (shiny) button while changed settings are not stored yet, or storing them failed
- Stored settings which do not fit the current input data (e.g. a track or attribute created via `renderUI` which is not part of the data anymore) are ignored and show their default values, together with the warning about not yet stored settings; if the user already changed settings while this App was starting, only the warning is shown (new exported function `ignoreNotApplicableSettings()`)

### Changed
- `saveBookmarkAsLatest()` and `notifyPushBookmark()` return (invisibly) whether the bookmark was saved/uploaded
- The shiny bookmark is only uploaded if it was saved successfully
- The WebSocket heartbeat and the JSON extraction are no longer stored in the shiny bookmark
- `input.json` is written (and uploaded) again once the (shiny) App finished starting, so that it also contains the settings created via `renderUI`

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