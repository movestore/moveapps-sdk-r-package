# CHANGELOG

All notable changes to the MoveApps R SDK will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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