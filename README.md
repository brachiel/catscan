# catscan
Automatic categorisation of scans

## Dependencies
### To run the scan stack
* Install `scanimage`; a console tool to automate scanning process with SANE. Packages can be found for most popular GNU/Linux distributions.
* Install `tesseract-ocr`; a free image recognition tool.

### To build the API service and config parser
* Install [stack](https://www.haskellstack.org/), a cross-platform program for developing Haskell projects.
* Run `make` to build catFlap and catConfig. The latter is needed for the scan stack even if you don't use the catFlap API

## Preparing Windows
catscan needs `bash` to run the scan stack and `make` to build. You can circumvent the need for make, but not the need for bash if you want to automate scanning. See the catFlap/README to find the manual commands needed to build.

## TODO
* Configurable category aliases
* Define area format
* Categorisation through areas

