# https://community.rstudio.com/t/
# trouble-including-image-jpeg-png-svg-etc-in-shiny-app-embedded-in-r-package/
# 56156
.onLoad <- function(libname, pkgname) {
    shiny::addResourcePath(
        prefix = "images",
        directoryPath = system.file("images", package = pkgname)
    )
}

.onUnload <- function(libname, pkgname) {
    shiny::removeResourcePath("images")
}
