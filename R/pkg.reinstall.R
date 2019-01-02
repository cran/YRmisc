

#' @name pkg.reinstall
#' @aliases pkg.reinstall
#' @title Reinstalled packages after updating R
#' @description Reinstall packages from pre-saved RDA file, using pkg.save(). If no path is given, the function will try to look in the current working directory.
#' @usage pkg.reinstall(path)
#' @param path :a path that contains saved RDA file
#' @examples path <- getwd()
#' pkg.save(path)
#' pkg.reinstall(path)


pkg.reinstall <- function(path){

  inspkgs <- NULL

    load("installed_pkgs.rda")
    tmp <- installed.packages()
    inspkgs.new <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
    missing <- setdiff(inspkgs, inspkgs.new)

    if(length(missing) > 0){

      install.packages(missing)
      update.packages()

      cat("Installed packages", missing)

    }else{

      cat("There is no package to be installed.")

    }

}

