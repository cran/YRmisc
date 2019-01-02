

#' @name pkg.save
#' @aliases pkg.save
#' @title Save installed packages
#' @description Save installed packages to an RDA file in a given directory. If no path is given, the file will be saved in the current working directory.
#' @usage pkg.save(path)
#' @param path :a path to save installed package file
#' @examples path <- getwd()
#' pkg.save(path)


pkg.save <- function(path){

    tmp <- installed.packages()
    inspkgs <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
    save(inspkgs, file="installed_pkgs.rda")

    cat("The packages are saved. Please use pkg.reinstall() after updating R to reinstall")

}
